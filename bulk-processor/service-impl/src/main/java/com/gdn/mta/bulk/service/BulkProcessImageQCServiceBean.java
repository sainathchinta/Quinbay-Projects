package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.repository.BulkProcessImageQCRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class BulkProcessImageQCServiceBean implements BulkProcessImageQCService {

    @Autowired
    private BulkProcessImageQCRepository bulkProcessImageQCRepository;

    @Override
    public List<BulkProcessImageQC> fetchAllBulkProcessImageQCByBulkProcess(String storeId,
        BulkProcess bulkProcess) {
        return bulkProcessImageQCRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(
            storeId, bulkProcess.getBulkProcessCode());
    }

    @Transactional(readOnly = false)
    @Override
    public void saveBulkProcessImageQc(List<BulkProcessImageQC> bulkProcessImageQCs) {
        bulkProcessImageQCRepository.saveAll(bulkProcessImageQCs);
    }

    @Override
    public BulkProcessImageQC fetchBulkProcessImageQCByBulkProcessCodeAndParentProduct(String storeId, String bulkProcessCode, String parentProduct) {
        return bulkProcessImageQCRepository.findByStoreIdAndBulkProcessCodeAndParentProductAndMarkForDeleteFalse(storeId, bulkProcessCode, parentProduct);
    }


}
