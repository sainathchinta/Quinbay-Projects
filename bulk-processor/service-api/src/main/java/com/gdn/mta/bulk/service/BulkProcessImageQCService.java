package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;

import java.util.List;

public interface BulkProcessImageQCService {
    /**
     * Find by storeId and bulk process code
     *
     * @param storeId
     * @param bulkProcess
     * @return
     */
    List<BulkProcessImageQC> fetchAllBulkProcessImageQCByBulkProcess(String storeId, BulkProcess bulkProcess);

    /**
     * Save bulkProcessImageQCs
     *
     * @param bulkProcessImageQCs
     */
    void saveBulkProcessImageQc(List<BulkProcessImageQC> bulkProcessImageQCs);

    BulkProcessImageQC fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(String storeId, String bulkProcessCode, String parentProduct);
}