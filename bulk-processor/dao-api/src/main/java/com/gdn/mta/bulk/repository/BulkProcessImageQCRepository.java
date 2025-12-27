package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface BulkProcessImageQCRepository extends JpaRepository<BulkProcessImageQC, String> {
    List<BulkProcessImageQC> findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(String storeId,
        String BulkProcessCode);

    BulkProcessImageQC findByStoreIdAndBulkProcessCodeAndParentProductAndMarkForDeleteFalse(
        String storeId, String bulkProcessCode, String parentProduct);
}