package com.gdn.mta.bulk.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.bulk.entity.BulkProcessVideo;

public interface BulkProcessVideoRepository extends JpaRepository<BulkProcessVideo, String> {

  List<BulkProcessVideo> findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(String storeId, String bulkProcessId);

  Optional<BulkProcessVideo> findByStoreIdAndBulkProcessCodeAndUploadedURL(String storeId, String bulkProcessCode,
      String uploadedURL);

}
