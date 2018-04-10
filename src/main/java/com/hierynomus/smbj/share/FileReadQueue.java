package com.hierynomus.smbj.share;

import com.hierynomus.mserref.NtStatus;
import com.hierynomus.mssmb2.SMBApiException;
import com.hierynomus.mssmb2.messages.SMB2ReadResponse;
import com.hierynomus.protocol.commons.concurrent.Futures;
import com.hierynomus.protocol.transport.TransportException;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

class FileReadQueue {
    private final File file;
    private final int bufferSize;
    private final int readaheadSize;
    private final long readTimeout;

    private long readOffset;
    private long pendingAmount;
    private List<Future<SMB2ReadResponse>> pendingRequests;

    public FileReadQueue(File file, int bufferSize, long readTimeout) {
        this.file = file;
        this.bufferSize = bufferSize;
        this.readaheadSize = bufferSize;
        this.readTimeout = readTimeout;

        pendingRequests = new LinkedList<>();

        reset(0);
    }

    public void reset(long position) {
        readOffset = position;
        clearPendingRequests();
    }

    private void clearPendingRequests() {
        for (Future<SMB2ReadResponse> pendingRequest : pendingRequests) {
            pendingRequest.cancel(true);
        }
        pendingRequests.clear();
        pendingAmount = 0;
    }

    public byte[] getNextChunk() throws IOException {
        if (pendingRequests.isEmpty()) {
            fillQueue();
        }

        SMB2ReadResponse response = getNextResponse();

        if (response != null) {
            fillQueue();
            return response.getData();
        } else {
            clearPendingRequests();
            return null;
        }
    }

    private SMB2ReadResponse getNextResponse() throws TransportException {
        if (pendingRequests.isEmpty()) {
            return null;
        }

        SMB2ReadResponse response = getReadResponse(pendingRequests.remove(0), readTimeout);
        if (response != null) {
            pendingAmount -= response.getDataLength();
        }
        return response;
    }

    private void fillQueue() throws IOException {
        while(pendingAmount < readaheadSize) {
            pendingRequests.add(sendReadRequest(readOffset, bufferSize));
            pendingAmount += bufferSize;
        }
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
}
