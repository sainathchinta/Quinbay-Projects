package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

import java.util.List;
import java.util.Map;

public interface NotificationService {

  void sendNotification(BulkProcess bulkProcess, String notificationType, boolean setNotificationDetail)
    throws Exception;

  void sendBulkAssignNotification(BulkProcess bulkProcess, int successCount, int totalCount,
    String vendorCode);

  void sendNotificationWithErrorFileGenerated(BulkProcess bulkProcess, String description, boolean isPromoUpdate, boolean isWholeSaleConfig);

  void sendDownloadNotification(BulkDownloadRequest request, boolean success,
      boolean partialAllDownload) throws Exception;

  void sendDownloadNotification(BulkDownloadEntity bulkDownloadEntity);

  void sendNeedRevisionDeletionNotification(BulkNeedRevisionDeletion bulkNeedRevisionDeletion, String fileUrl,
      int totalCount, int nextWeekDeletionSize);

  void sendBulkUploadedNotification(BulkProcess bulkProcess, String notificationType, String failedProductLink);

  void sendGenerateQrCodeNotification(BulkProcess bulkProcess, String filePath);

  void sendGenerateQrCodeFailedNotification(String merchantCode, String qrGenerationType);

  void sendNotificationForSchedulesRemovalForUpsetAndUpdate(
    Map<String, List<BulkProcessData>> productSkuXBulkProcessDataListGrouped, String businessPartnerCode);

  void sendBulkUploadedNotificationWithoutErrorSheet(BulkProcess bulkProcess,
    String notificationType);
}
