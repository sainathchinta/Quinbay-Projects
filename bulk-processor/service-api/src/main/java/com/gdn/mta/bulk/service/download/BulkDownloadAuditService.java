package com.gdn.mta.bulk.service.download;

import java.util.List;

import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

import org.springframework.transaction.annotation.Transactional;

/**
 * Created by keshashah on 04/11/16.
 */
public interface BulkDownloadAuditService {

  /**
   * Create Audit for Bulk Download Process
   *
   * @param request
   * @param status
   */
  void createAuditLog(BulkDownloadRequest request, String status) throws Exception;

  @Transactional
  void updateAuditLog(String requestId, String status, int recordsDownloaded, String errorMessage) throws Exception;

  /**
   * Api to get Pending AuditLogs
   *
   * @param limit
   * @param status
   */
  List<BulkDownloadEntity> getPendingAuditLogs(String status, int limit) throws Exception;

  /**
   * Api to save BulkDownloadEntity
   *
   * @param bulkDownloadEntity
   */
  BulkDownloadEntity saveBulkDownloadEntity(BulkDownloadEntity bulkDownloadEntity);

  /**
   * Save list of bulk download entities
   *
   * @param bulkDownloadEntityList
   * @return
   */
  List<BulkDownloadEntity> saveBulkDownloadEntityList(List<BulkDownloadEntity> bulkDownloadEntityList);
}
