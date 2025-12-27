package com.gdn.mta.bulk.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImage;

public interface BulkProcessImageRepository extends JpaRepository<BulkProcessImage, String> {

  List<BulkProcessImage> findByStoreIdAndBulkProcessIdAndMarkForDeleteFalse(String storeId, String bulkProcessId);

  List<BulkProcessImage> findByBulkProcessIdAndImageURLIn(String bulkProcessId, List<String> imageUrls);

  List<BulkProcessImage> findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(String storeId, String bulkProcessCode);

  @Modifying
  @Query(value = "Delete from blp_bulk_process_image where store_id = :storeId and bulk_process_code = :bulkProcessCode", nativeQuery = true)
  void deleteByBulkProcessCode(@Param("storeId") String storeId, @Param("bulkProcessCode") String bulkProcessCode);

  @Modifying
  @Query(value = "update blp_bulk_process_image set completed = true, updated_date = CURRENT_TIMESTAMP, updated_by = 'RUNDECK', "
      + "error_message = ?4 where store_id = ?1 and bulk_process_code in (?2) and "
      + "updated_date < ?3 and completed = false", nativeQuery = true)
  void updatePendingImageDownloads(String storeId, List<String> bulkProcessCodes, Date updatedDate, String errorMessage);
}
