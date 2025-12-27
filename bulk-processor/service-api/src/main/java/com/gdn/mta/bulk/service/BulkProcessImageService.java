package com.gdn.mta.bulk.service;

import java.util.Date;
import java.util.List;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImage;

public interface BulkProcessImageService {

  /**
   * Fetch BulkProcessImage by bulk process code and image url
   *
   * @param bulkProcess
   * @param imageUrlList
   * @return
   */
  List<BulkProcessImage> findByBulkProcessCodeAndImageUrl(BulkProcess bulkProcess, List<String> imageUrlList);

  /**
   * Save bulk process image list
   *
   * @param bulkProcessImages
   */
  void saveBulkProcessImage(List<BulkProcessImage> bulkProcessImages);

  /**
   * Find by storeId and bulk process code
   *
   * @param storeId
   * @param bulkProcess
   * @return
   */
  List<BulkProcessImage> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess);

  /**
   * Find by storeId and bulk process code
   *
   * @param storeId
   * @param bulkProcessCodes
   * @return
   */
  void updatePendingImageDownloads(String storeId, List<String> bulkProcessCodes, Date updatedDate);

  /**
   * delete image by updated date
   *
   * @param bulkProcessCode
   * @param storeId
   * @throws Exception
   */
  void deleteImageByBulkProcessCode(String storeId, String bulkProcessCode);
}
