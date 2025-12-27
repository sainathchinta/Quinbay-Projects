package com.gdn.mta.bulk.service;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.repository.BulkProcessImageRepository;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Service
@Transactional(readOnly = true)
@Slf4j
public class BulkProcessImageServiceBean implements BulkProcessImageService {

  @Autowired
  private BulkProcessImageRepository bulkProcessImageRepository;

  @Override
  public List<BulkProcessImage> findByBulkProcessCodeAndImageUrl(BulkProcess bulkProcess, List<String> imageUrlList) {
    List<BulkProcessImage> bulkProcessImageList =
        bulkProcessImageRepository.findByBulkProcessIdAndImageURLIn(bulkProcess.getId(), imageUrlList);
    return bulkProcessImageList.stream().filter(bulkProcessImage -> !bulkProcessImage.isCompleted())
        .collect(Collectors.toList());
  }

  @Override
  @Transactional(readOnly = false)
  public void saveBulkProcessImage(List<BulkProcessImage> bulkProcessImages) {
    bulkProcessImageRepository.saveAll(bulkProcessImages);
  }

  @Override
  public List<BulkProcessImage> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess) {
    return bulkProcessImageRepository.findByStoreIdAndBulkProcessIdAndMarkForDeleteFalse(storeId, bulkProcess.getId());
  }

  @Override
  @Transactional(readOnly = false)
  public void updatePendingImageDownloads(String storeId, List<String> bulkProcessCodes, Date updatedDate) {
    log.info("Updating the complete status in blp_bulk_process_image for bulk process codes : {} and updatedDate : {}",
        bulkProcessCodes, updatedDate);
    bulkProcessImageRepository.updatePendingImageDownloads(storeId, bulkProcessCodes, updatedDate,
        Constant.SYSTEM_ERROR);
  }

  @Override
  public void deleteImageByBulkProcessCode(String storeId, String bulkProcessCode) {
    this.bulkProcessImageRepository.deleteByBulkProcessCode(storeId, bulkProcessCode);
  }
}
