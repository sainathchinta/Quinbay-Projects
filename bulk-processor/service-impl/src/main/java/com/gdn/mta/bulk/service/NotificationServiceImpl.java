package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.dto.BulkProcessType.ASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.DISASSEMBLY_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.TRANSFER_REQUEST;
import static com.gdn.mta.bulk.dto.BulkProcessType.getBulkProcessType;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.GenerateQrCodeNotificationModel;
import com.gdn.partners.bulk.util.NotificationTypeConstant;
import com.google.common.collect.ImmutableSet;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.ApplicationProperties;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.google.common.collect.ImmutableMap;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class NotificationServiceImpl implements NotificationService {
  private static final String STORE = "Store";
  private static final String TOKO = "toko";
  private static final String PRODUCT = "Product";
  private static final String PRODUK = "produk";
  public static final String PRODUCTS_HAS_BEEN_DELETED = " products has been deleted";
  public static final String PRODUCTS_WILL_BE_AUTOMATICALLY_DELETED_IN_7_DAYS =
      " products will be automatically deleted in 7 days";

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${update.schedules.removal.enabled}")
  private boolean updateScheduleRemovalEnabled;

  @Value("${need.revision.page.url}")
  private String needRevisionPageUrl;

  @Value("${max.L5.download.size}")
  private int maxL5DownloadSize;


  private static final ImmutableMap<BulkProcessEntity, String> FILE_NAME_MAP =
      new ImmutableMap.Builder<BulkProcessEntity, String>()
          .put(BulkProcessEntity.PRODUCT, Constant.FILE_BULK_UPDATE_PRODUCT_TEMPLATE)
          .put(BulkProcessEntity.PRODUCT_EAN,Constant.FILE_BULK_UPDATE_PRODUCT_EAN_TEMPLATE)
          .put(BulkProcessEntity.ORDER, Constant.FILE_BULK_ORDER_DOWNLOAD_TEMPLATE)
          .put(BulkProcessEntity.PRODUCT_VENDOR, Constant.FILE_BULK_PRODUCT_VENDOR_TEMPLATE)
          .put(BulkProcessEntity.CAMPAIGN_PRODUCT, Constant.FILE_CAMPAIGN_PRODUCT_LIST)
          .put(BulkProcessEntity.MASTER_PRODUCT, Constant.FILE_BULK_UPDATE_PRODUCT_TEMPLATE_FOR_INTERNAL)
          .put(BulkProcessEntity.INSTANT_PICKUP_PRODUCT, Constant.FILE_BULK_UPLOAD_OFFLINE_ITEMS_TEMPLATE)
          .put(BulkProcessEntity.PRODUCT_BASIC_INFO, Constant.FILE_BULK_UPDATE_PRODUCT_BASIC_INFO_TEMPLATE)
          .build();

  private static final Set<BulkProcessType> BULK_WORK_ORDER_UPLOAD_TYPE =
    ImmutableSet.of(DISASSEMBLY_REQUEST, ASSEMBLY_REQUEST, TRANSFER_REQUEST);

  @Override
  public void sendNotification(BulkProcess bulkProcess, String notificationType,
    boolean setNotificationDetail) throws Exception {
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(null,
        bulkProcess.getDescription(), notificationType, bulkProcess.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    if (setNotificationDetail) {
      notificationKafka.setNotificationDetail(bulkProcess.getBulkProcessCode());
    }
    notificationKafka.setAppNames(new HashSet<>(Collections.singletonList(Constant.CLIENT_ID)));
    if (StringUtils.isNotBlank(notificationKafka.getDestinationKey())) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          bulkProcess.getBusinessPartnerCode(), notificationKafka);
      log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    } else {
      log.info("Skipping sending bulk notification for bulkProcessCode : {}", bulkProcess.getBulkProcessCode());
    }
  }

  @Override
  public void sendBulkAssignNotification(BulkProcess bulkProcess, int successCount, int totalCount,
    String vendorCode) {
    String notificationMessage;
    if (totalCount == successCount) {
      notificationMessage = Constant.ALL_PRODUCTS_SUCCESSFULLY_ASSIGNED;
    } else {
      notificationMessage = new StringBuilder().append(successCount).append(Constant.OF)
          .append(totalCount).append(Constant.FEW_PRODUCTS_SUCCESSFULLY_ASSIGNED)
          .toString();
    }
    generateAndSendNotification(bulkProcess, NotificationType.BULK_UPDATED.getValue(), false,
      notificationMessage, vendorCode);
  }

  @Override
  public void sendNotificationWithErrorFileGenerated(BulkProcess bulkProcess, String description, boolean isPromoUpdate,
      boolean isWholeSaleConfig) {
    String notificationType;
    BulkProcessType processType = getBulkProcessType(bulkProcess.getBulkProcessType());
    if(isPromoUpdate) {
      notificationType = NotificationType.BULK_UPDATED_WITH_PROMO.getValue();
    } else {
      notificationType = NotificationType.BULK_UPDATED.getValue();
    }
    if (isWholeSaleConfig) {
      notificationType = NotificationType.WHOLE_SALE_BULK_UPDATE.getValue();
    }
    if(BULK_WORK_ORDER_UPLOAD_TYPE.contains(processType)){
      notificationType = NotificationType.BULK_UPLOADED.getValue();
    }
    generateAndSendNotification(bulkProcess, notificationType,
        isPromoUpdate || isWholeSaleConfig ||
            !StringUtils.equals(BulkProcess.STATUS_FINISHED, bulkProcess.getStatus()),
      description, bulkProcess.getBusinessPartnerCode());
  }

  @Override
  public void sendDownloadNotification(BulkDownloadRequest request, boolean success,
      boolean partialAllDownload) throws Exception {
    String notificationDetail = null;
    String notificationMessage = null;
    String notificationType;
    if (success) {
      BulkProcessEntity entity = request.getBulkProcessEntity();
      String fileName = Constant.DEFAULT_FILE_NAME;
      if (entity != null && FILE_NAME_MAP.containsKey(entity)) {
        fileName = FILE_NAME_MAP.get(entity);
        if (BulkProcessEntity.CAMPAIGN_PRODUCT.name().equals(entity.name())) {
          fileName = fileName + request.getMerchantId();
        }
      }
      if (!request.isDirectDownload()) {
        notificationMessage = new StringBuilder().append(Constant.FILE_CREATION_MESSAGE).append(fileName)
                .append(Constant.SEND_EMAIL_MESSAGE).toString();
      } else {
        notificationDetail = fileStorageService.getNotificationDetailPath(request);
        notificationMessage =
          new StringBuilder().append(Constant.FILE_CREATION_MESSAGE).append(fileName)
            .append(partialAllDownload ? String.format(Constant.PARTIAL_DOWNLOAD_MESSAGE, maxL5DownloadSize) : Constant.DOWNLOAD_MESSAGE).toString();
      }
    } else {
      notificationMessage = Constant.FAILED_MESSAGE;
    }
    notificationType = fileStorageService.getNotificationType(request.getBulkProcessEntity());
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        notificationDetail, notificationMessage, notificationType, request.getMerchantId());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    if (StringUtils.isNotBlank(notificationKafka.getDestinationKey())) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(), request.getMerchantId(),
          notificationKafka);
      log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    } else {
      log.info("Skipping sending bulk notification for bulk download request : {}", request);
    }
  }

  @Override
  public void sendDownloadNotification(BulkDownloadEntity bulkDownloadEntity) {
    String notificationMessage;
    if(Constant.SUCCESS.equalsIgnoreCase(bulkDownloadEntity.getStatus())) {
      notificationMessage = new StringBuilder().append("Penciptaan file ").append(bulkDownloadEntity.getFileName())
          .append(" berhasil. Silakan cek email untuk mendownload file.").toString();
    } else {
      notificationMessage = "Penciptaan file gagal. Silakan coba beberapa saat lagi.";
    }
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(null,
        notificationMessage, NotificationType.BULK_DOWNLOADED.getValue(), bulkDownloadEntity.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    if (StringUtils.isNotBlank(notificationKafka.getDestinationKey())) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          bulkDownloadEntity.getBusinessPartnerCode(), notificationKafka);
      log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    } else {
      log.info("Skipping sending bulk notification for bulkDownloadEntity : {}", bulkDownloadEntity);
    }
  }

  @Override
  public void sendNeedRevisionDeletionNotification(BulkNeedRevisionDeletion needRevisionDeletion, String fileUrl,
      int eligibleForDeletionSize, int nextWeekDeletionSize) {
    String notificationMessage = eligibleForDeletionSize + PRODUCTS_HAS_BEEN_DELETED;
    NotificationKafka notificationKafka =
        ConverterUtil.getNotificationKafka(null, notificationMessage, NotificationTypeConstant.DOWNLOAD_DELETED_NEED_CORRECTION_PRODUCTS,
            needRevisionDeletion.getBusinessPartnerCode());
    String notificationMessageForNextWeek = nextWeekDeletionSize + PRODUCTS_WILL_BE_AUTOMATICALLY_DELETED_IN_7_DAYS;
    NotificationKafka notificationKafkaForNextWeekDeletedProducts =
        ConverterUtil.getNotificationKafka(null, notificationMessageForNextWeek, NotificationTypeConstant.SEND_FOR_NEED_CORRECTION_FOR_PRODUCT_LIMITS,
            needRevisionDeletion.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    setMandatoryParamsToNotificationKafka(notificationKafkaForNextWeekDeletedProducts);
    notificationKafka.setNotificationDetail(fileUrl);
    notificationKafkaForNextWeekDeletedProducts.setNotificationDetail(needRevisionPageUrl);
    if (eligibleForDeletionSize > 0) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          needRevisionDeletion.getBusinessPartnerCode(), notificationKafka);
    }
    if (nextWeekDeletionSize > 0) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          needRevisionDeletion.getBusinessPartnerCode(), notificationKafkaForNextWeekDeletedProducts);
    }
  }

  @Override
  public void sendBulkUploadedNotification(BulkProcess bulkProcess, String notificationType, String failedProductLink) {
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(null,
        bulkProcess.getDescription(), notificationType, bulkProcess.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    if (!BulkProcess.STATUS_FINISHED.equals(bulkProcess.getStatus())) {
      notificationKafka.setNotificationDetail(bulkProcess.getBulkProcessCode());
      if(!StringUtils.isBlank(failedProductLink)) {
        notificationKafka.setNotificationMessage(notificationKafka.getNotificationMessage() + " " + failedProductLink);
      }
    }
    if (StringUtils.isNotBlank(notificationKafka.getDestinationKey())) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          bulkProcess.getBusinessPartnerCode(), notificationKafka);
      log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    } else {
      log.info("Skipping sending bulk notification for bulkProcessCode : {}", bulkProcess.getBulkProcessCode());
    }
  }

  @Override
  public void sendGenerateQrCodeNotification(BulkProcess bulkProcess, String filePath) {

    GenerateQrCodeNotificationModel event = new GenerateQrCodeNotificationModel();
    event.setMerchantCode(bulkProcess.getBusinessPartnerCode());
    event.setRedirectionUrl(filePath);

    if (AllowedQRGenerationType.STORE.getValue().equals(bulkProcess.getDescription())
        || AllowedQRGenerationType.ALL_PRODUCTS.getValue().equals(bulkProcess.getDescription())) {
      event.setQrGenerationTypeEn(STORE);
      event.setQrGenerationTypeIn(TOKO);
    } else {
      event.setQrGenerationTypeEn(PRODUCT);
      event.setQrGenerationTypeIn(PRODUK);
    }

    kafkaProducer.send(applicationProperties.getGenerateQrCodePushNotificationTopic(),
        bulkProcess.getBusinessPartnerCode(), event);
    log.info("Sent object for QR generation. bulkNotificationQueue: {} ", event);
  }

  @Override
  public void sendGenerateQrCodeFailedNotification(String merchantCode, String qrGenerationType) {
    GenerateQrCodeNotificationModel event = new GenerateQrCodeNotificationModel();
    event.setMerchantCode(merchantCode);

    if (AllowedQRGenerationType.STORE.getValue().equals(qrGenerationType)) {
      event.setQrGenerationTypeEn(STORE);
      event.setQrGenerationTypeIn(TOKO);
    } else {
      event.setQrGenerationTypeEn(PRODUCT);
      event.setQrGenerationTypeIn(PRODUK);
    }
    kafkaProducer.send(applicationProperties.getFailureQrCodePushNotificationTopic(), merchantCode, event);
    log.info("Sent object for QR generation. bulkNotificationQueue: {} ", event);
  }

  @Override
  public void sendNotificationForSchedulesRemovalForUpsetAndUpdate(
    Map<String, List<BulkProcessData>> productSkuXBulkProcessDataListGrouped, String businessPartnerCode) {
    for(Map.Entry<String, List<BulkProcessData>> entry :
      productSkuXBulkProcessDataListGrouped.entrySet()){
      String productSku = entry.getKey();
      String notificationMessageForScheduleRemoval = entry.getValue().stream().map(BulkProcessData::getIdentifier)
        .filter(StringUtils::isNotBlank).findFirst().orElse(StringUtils.EMPTY);
      NotificationKafka notificationKafka =
        ConverterUtil.getNotificationKafka(productSku, notificationMessageForScheduleRemoval,
          NotificationType.PRODUCT_ACTIVATED.getValue(), businessPartnerCode);
      if(StringUtils.isNotBlank(notificationKafka.getDestinationKey())){
        log.info("Sending Schedule removal notification for : {} with message : {} ", productSku,
          notificationKafka);
        kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          businessPartnerCode, notificationKafka);
      }
    }
  }

  @Override
  public void sendBulkUploadedNotificationWithoutErrorSheet(BulkProcess bulkProcess,
    String notificationType) {
    NotificationKafka notificationKafka =
      ConverterUtil.getNotificationKafka(null, bulkProcess.getDescription(), notificationType,
        bulkProcess.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
      bulkProcess.getBusinessPartnerCode(), notificationKafka);
  }

  private void generateAndSendNotification(BulkProcess bulkProcess, String notificationType,
      boolean setNotificationDetail, String notificationMessage, String destinationKey) {
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(null,
        bulkProcess.getDescription(), notificationType, destinationKey);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    if (setNotificationDetail) {
      notificationKafka.setNotificationDetail(bulkProcess.getBulkProcessCode());
    }
    if(!StringUtils.isBlank(notificationMessage)) {
      notificationKafka.setNotificationMessage(notificationMessage);
    }
    notificationKafka.setAppNames(new HashSet<>(Collections.singletonList(Constant.CLIENT_ID)));
    if (StringUtils.isNotBlank(notificationKafka.getDestinationKey())) {
      kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
          bulkProcess.getBusinessPartnerCode(), notificationKafka);
      log.info("Sent object for Queue processing. bulkNotificationQueue: {} ", notificationKafka);
    } else {
      log.info("Skipping sending bulk notification for bulkProcessCode : {}", bulkProcess.getBulkProcessCode());
    }
  }

  private void setMandatoryParamsToNotificationKafka(NotificationKafka notificationKafka) {
    notificationKafka.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
    notificationKafka.setRequestId(GdnMandatoryRequestParameterUtil.getRequestId());
    notificationKafka.setChannelId(GdnMandatoryRequestParameterUtil.getChannelId());
    notificationKafka.setUsername(GdnMandatoryRequestParameterUtil.getUsername());
    notificationKafka.setClientId(GdnMandatoryRequestParameterUtil.getClientId());
    if(StringUtils.isBlank(notificationKafka.getStoreId())) {
      notificationKafka.setStoreId(Constant.STORE_ID);
    }
  }

}
