package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import org.springframework.data.domain.Pageable;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.jpa.repository.JpaRepository;


public interface NeedRevisionDeletionRepository extends JpaRepository<BulkNeedRevisionDeletion,String> {

  List<BulkNeedRevisionDeletion> findByStoreIdAndStatusAndProcessTypeOrderByUpdatedDateAsc(
    String storeId, String processType, String status, Pageable pageable);

  Page<BulkNeedRevisionDeletion> findByStoreIdAndProcessTypeAndStatus(String storeId, String ProcessType, String status,
      Pageable pageable);

  Page<BulkNeedRevisionDeletion> findByStoreIdAndStatusAndMarkForDeleteFalseOrderByCreatedDateAsc(
      String storeId, String status, Pageable pageable);

  BulkNeedRevisionDeletion findByStoreIdAndDeletionProcessCode(String storeId,
      String deletionProcessCode);

  List<BulkNeedRevisionDeletion> findByStoreIdAndStatusOrderByCreatedDateAsc(
      String storeId, String status, Pageable pageable);
}
