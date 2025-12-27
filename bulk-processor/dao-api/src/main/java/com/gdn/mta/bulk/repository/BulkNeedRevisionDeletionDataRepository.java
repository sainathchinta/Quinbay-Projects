package com.gdn.mta.bulk.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;

public interface BulkNeedRevisionDeletionDataRepository extends JpaRepository<BulkNeedRevisionDeletionData, String> {
List<BulkNeedRevisionDeletionData> findByStoreIdAndDeletionProcessCode(String StoreId,String deletionProcessCode);

  List<BulkNeedRevisionDeletionData> findByStoreIdAndDeletionProcessCodeAndIdIn(String storeId,
      String deletionProcessCode, List<String> needRevisionDeletionDataIds);

  List<BulkNeedRevisionDeletionData> findDistinctDeletionProcessCodeByStoreIdAndDeletionProcessCodeInAndStatusIn(String storeId,
      List<String> deletionProcessCodes, List<String> statuses);
}
