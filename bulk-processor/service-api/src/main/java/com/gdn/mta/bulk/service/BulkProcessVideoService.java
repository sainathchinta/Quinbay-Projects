package com.gdn.mta.bulk.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessVideo;

public interface BulkProcessVideoService {
  /**
   * Find by storeId and bulk process code
   *
   * @param storeId
   * @param bulkProcess
   * @return
   */
  List<BulkProcessVideo> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess);

  /**
   * Save all bulk process video list
   *
   * @param bulkProcessVideoList List<BulkProcessVideo>
   * @return List<BulkProcessVideo>
   */
  List<BulkProcessVideo> saveAllBulkProcessVideo(List<BulkProcessVideo> bulkProcessVideoList);

  /**
   * Find by storeId and bulk process code and url
   *
   * @param storeId String
   * @param bulkProcessCode String
   * @param uploadedUrl String
   * @return BulkProcessVideo
   */
  BulkProcessVideo findByStoreIdAndBulkProcessCodeAndUploadedURL(String storeId, String bulkProcessCode,
      String uploadedUrl);

  /**
   * Save bulk process video
   *
   * @param bulkProcessVideo bulkProcessVideo
   * @return bulkProcessVideo
   */
  BulkProcessVideo saveBulkProcessVideo(BulkProcessVideo bulkProcessVideo);
}
