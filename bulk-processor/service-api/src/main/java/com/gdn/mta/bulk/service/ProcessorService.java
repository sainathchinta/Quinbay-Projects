package com.gdn.mta.bulk.service;

import java.util.Map;

import com.gdn.mta.bulk.entity.BulkProcessQueue;

public interface ProcessorService {

  void preProcess(String storeId, String requestId, String bulkProcessType, String businessPartnerCode,
    Map<String, String> files, Map<String, String> args, String bulkProcessCode, String username) throws Exception;

  void process(BulkProcessQueue bulkProcessQueue) throws Exception;

}
