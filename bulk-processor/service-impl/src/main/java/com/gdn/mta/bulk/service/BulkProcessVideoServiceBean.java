package com.gdn.mta.bulk.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.repository.BulkProcessVideoRepository;

@Service
public class BulkProcessVideoServiceBean implements BulkProcessVideoService {

  @Autowired
  private BulkProcessVideoRepository bulkProcessVideoRepository;

  @Override
  public List<BulkProcessVideo> findByStoreIdAndBulkProcess(String storeId, BulkProcess bulkProcess) {
    return bulkProcessVideoRepository.findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(storeId,
        bulkProcess.getBulkProcessCode());
  }

  @Override
  @Transactional(readOnly = false)
  public List<BulkProcessVideo> saveAllBulkProcessVideo(List<BulkProcessVideo> bulkProcessVideoList) {
    return bulkProcessVideoRepository.saveAll(bulkProcessVideoList);
  }

  @Override
  public BulkProcessVideo findByStoreIdAndBulkProcessCodeAndUploadedURL(String storeId, String bulkProcessCode,
      String uploadedUrl) {
    return bulkProcessVideoRepository.findByStoreIdAndBulkProcessCodeAndUploadedURL(storeId, bulkProcessCode,
        uploadedUrl).orElse(null);
  }

  @Override
  @Transactional
  public BulkProcessVideo saveBulkProcessVideo(BulkProcessVideo bulkProcessVideo) {
    return bulkProcessVideoRepository.save(bulkProcessVideo);
  }
}
