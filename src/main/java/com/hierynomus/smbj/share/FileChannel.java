package com.hierynomus.smbj.share;

import com.hierynomus.mserref.NtStatus;
import com.hierynomus.msfscc.fileinformation.FileEndOfFileInformation;
import com.hierynomus.msfscc.fileinformation.FileStandardInformation;
import com.hierynomus.mssmb2.SMBApiException;
import com.hierynomus.mssmb2.messages.SMB2ReadResponse;
import com.hierynomus.protocol.commons.concurrent.Futures;
import com.hierynomus.protocol.transport.TransportException;
import com.hierynomus.smbj.io.ByteChunkProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

public class FileChannel implements SeekableByteChannel {
    private File file;
    private final long readTimeout;
    private boolean isClosed;

    private ByteBuffer buf;
    private FileReadQueue readQueue;

    private long position = 0;

    private Future<SMB2ReadResponse> nextResponse;

    private static final Logger logger = LoggerFactory.getLogger(FileInputStream.class);
    private int bufferSize;

    FileChannel(File file, int bufferSize, long readTimeout) {
        this.file = file;
        this.bufferSize = bufferSize;
        this.readTimeout = readTimeout;
        this.readQueue = new FileReadQueue(file, bufferSize, readTimeout);
    }

    private void checkOpen() throws ClosedChannelException {
        if (isClosed) {
            throw new ClosedChannelException();
        }
    }

    @Override
    public boolean isOpen() {
        return !isClosed;
    }

    @Override
    public void close() throws IOException {
        isClosed = true;
        file = null;
        buf = null;
    }

    @Override
    public long position() throws IOException {
        checkOpen();
        return position;
    }

    @Override
    public FileChannel position(long newPosition) throws IOException {
        checkOpen();
        if (newPosition != position) {
            updatePosition(newPosition);
        }
        return this;
    }

    private void updatePosition(long newPosition) {
        position = newPosition;
        buf = null;
        readQueue.reset(newPosition);
    }

    @Override
    public long size() throws IOException {
        checkOpen();
        return file.getFileInformation(FileStandardInformation.class).getEndOfFile();
    }

    @Override
    public FileChannel truncate(long size) throws IOException {
        checkOpen();
        file.setFileInformation(new FileEndOfFileInformation(size));
        return this;
    }

    @Override
    public int read(ByteBuffer dst) throws IOException {
        checkOpen();

        int totalRead = 0;

        while(dst.remaining() > 0) {
            if (buf == null || buf.remaining() <= 0) {
                long amountRead = loadBuffer();
                if (amountRead == -1) {
                    return -1;
                }
            }

            dst.put(buf);
        }

        position += totalRead;

        return totalRead;
    }

    public long transferTo(long position, long count, WritableByteChannel target) throws IOException {
        checkOpen();

        int bufferSize = file.share.session.getConnection().getNegotiatedProtocol().getMaxReadSize();
        if (count < bufferSize) {
            bufferSize = (int)count;
        }

        long offset = position;
        long remaining = count;

        FileReadQueue readQueue = new FileReadQueue(file, remaining > bufferSize ? bufferSize : (int) remaining, readTimeout);
        readQueue.reset(position);

        while (remaining > 0) {
            byte[] chunk = readQueue.getNextChunk();
            if (chunk == null) {
                break;
            }

            int dataLength = chunk.length;
            offset += dataLength;
            remaining -= dataLength;

            target.write(ByteBuffer.wrap(chunk));
        }

        return offset - position;
    }

    private Future<SMB2ReadResponse> createReadRequest(long offset, int length) throws IOException {
        return length > 0 ? sendReadRequest(offset, length) : null;
    }

    @Override
    public int write(final ByteBuffer src) throws IOException {
        checkOpen();
        int amountWritten = file.write(new ByteBufferChunkProvider(src, position));
        updatePosition(position + amountWritten);
        return amountWritten;
    }

    public long transferFrom(ReadableByteChannel src, long position, long count) throws IOException {
        checkOpen();

        int bufferSize = file.share.session.getConnection().getNegotiatedProtocol().getMaxWriteSize();
        if (count < bufferSize) {
            bufferSize = (int)count;
        }

        ByteBuffer writeBuffer = ByteBuffer.allocate(bufferSize);
        long offset = position;
        long remaining = count;

        while(remaining > 0) {
            writeBuffer.clear();
            src.read(writeBuffer);
            writeBuffer.flip();

            int bytesWritten = file.write(new ByteBufferChunkProvider(writeBuffer, offset));

            offset += bytesWritten;
            remaining -= bytesWritten;
        }

        return offset - position;
    }

    private long loadBuffer() throws IOException {
        byte[] chunk = readQueue.getNextChunk();
        if (chunk == null) {
            return -1;
        }

        int dataLength = chunk.length;
        buf = ByteBuffer.wrap(chunk);
        return dataLength;

    }

    private Future<SMB2ReadResponse> sendReadRequest(long offset, int length) throws IOException {
        return file.readAsync(offset, length);
    }

    private SMB2ReadResponse getReadResponse(Future<SMB2ReadResponse> request, long timeout) throws TransportException {
        SMB2ReadResponse res = Futures.get(request, timeout, TimeUnit.MILLISECONDS, TransportException.Wrapper);
        if (res.getHeader().getStatus() == NtStatus.STATUS_END_OF_FILE) {
            return null;
        }

        if (res.getHeader().getStatus() != NtStatus.STATUS_SUCCESS) {
            throw new SMBApiException(res.getHeader(), "Read failed for " + this);
        }

        return res;
    }

    private static class ByteBufferChunkProvider extends ByteChunkProvider {
        private final ByteBuffer src;

        public ByteBufferChunkProvider(ByteBuffer src, long startPosition) {
            this.src = src;
            this.offset = startPosition;
        }

        @Override
        public boolean isAvailable() {
            return src.remaining() > 0;
        }

        @Override
        protected int getChunk(byte[] chunk) throws IOException {
            int amount = Math.min(chunk.length, src.remaining());
            src.get(chunk, 0, amount);
            return amount;
        }

        @Override
        public int bytesLeft() {
            return src.remaining();
        }
    }
}
