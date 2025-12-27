package com.gdn.partners.pbp.service.notification;

import static com.gdn.partners.pbp.commons.constants.Constants.BUYABLE_LINK_STATUS;
import static com.gdn.partners.pbp.commons.constants.Constants.UPCOMING_STATUS;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductStockAlertNotification;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;
import com.newrelic.api.agent.Trace;

@Service
public class ProductNotificationServiceBean implements ProductNotificationService {

  private static final String NOTIFICATION_DETAIL_STOCK_ALERT = "stock_alert";
  private static final String APP_NAME = "PBP";
  private static final String PRODUCT_REJECTED_NOTIFICATION = "Product is rejected.\"";
  private static final String PRODUCT_RETURN_FOR_CORRECTION_NOTIFICATION = "Product need revision.\"%s\"";
  private static final String PRODUCT_ACTIVE_CATEGORY_CHANGE = "Product is active Blibli SKU:\"%s\". "
      + "We've made changes in the product category that affects product margin from %s%% to %s%%.";
  private static final String NOTIFICATION_END = "\".";
  private static final String PRODUCT_ACTIVE = "Product is now active. \"%s\".Blibli SKU %s";
  private static final String PRODUCT_STATUS = "The status for %s product has been changed to %s.";
  private static final String CATEGORY_CHANGE_EN = "Wrong Category";
  private static final String CATEGORY_CHANGE_IN = "Kategori tidak betul";
  private static final String QR_CODE_DOWNLOAD = "File %s berhasil di-download. Silakan download hasilnya";
  private static final String DELIMETER = "/";
  private static final String RESTRICTED_KEY_MESSAGE_EN =
      "doesn't match our standardization. We will review the product before it goes live on Blibli.";
  private static final String RESTRICTED_KEY_MESSAGE_IN =
      "belum memenuhi standar yang ditentukan. Kami akan me-review produk tersebut sebelum tampil di Blibli.";

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  public void sendProductActiveNotification(String businessPartnerCode, String gdnSku)
      throws Exception {
    StringBuilder message = new StringBuilder();
    message.append("Produk Anda telah aktif dengan Blibli Sku : ").append(gdnSku);
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(gdnSku,
        message.toString(), NotificationType.PRODUCT_ACTIVATED.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  public void sendProductStockMinimalNotification(String businessPartnerCode, Integer productCount)
      throws Exception {
    StringBuilder message = new StringBuilder();
    message.append("Terdapat ").append(productCount).append(" produk dengan stok minimum.");
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(NOTIFICATION_DETAIL_STOCK_ALERT,
        message.toString(), NotificationType.STOCK_MINIMUM_ALERT.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(DomainEventName.PRODUCT_MINIMUM_STOCK_NOTIFICATION,
        this.buildProductStockAlertModel(businessPartnerCode, productCount));
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  public void sendProductStockOosNotification(String businessPartnerCode, Integer productCount)
      throws Exception {
    StringBuilder message = new StringBuilder();
    message.append("Terdapat ").append(productCount).append(" produk dengan stok 0.");
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(NOTIFICATION_DETAIL_STOCK_ALERT,
        message.toString(), NotificationType.STOCK_OOS_ALERT.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(DomainEventName.PRODUCT_OOS_STOCK_NOTIFICATION,
        this.buildProductStockAlertModel(businessPartnerCode, productCount));
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  @Async
  public void sendProductRejectNotification(String businessPartnerCode, String productName)
      throws Exception {
    String message = new StringBuilder(PRODUCT_REJECTED_NOTIFICATION).append(productName)
        .append(NOTIFICATION_END).toString();
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(null,
        message, NotificationType.PRODUCT_REJECTED.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  @Async
  public void sendProductReturnForCorrectionNotification(String businessPartnerCode,
      String productName, String gdnSku, String notes) throws Exception {
    String message = String.format(PRODUCT_RETURN_FOR_CORRECTION_NOTIFICATION, productName);
    String notificationType;
    String correctionReasons = notes.substring(0, notes.indexOf('-'));
    if (correctionReasons.contains(CATEGORY_CHANGE_EN) || correctionReasons.contains(CATEGORY_CHANGE_IN)) {
      notificationType = NotificationType.PRODUCT_SEND_FOR_CORRECTION_CATEGORY_CHANGE.getValue();
    } else {
      notificationType = NotificationType.PRODUCT_SEND_FOR_CORRECTION.getValue();
    }
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        gdnSku, message, notificationType, businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  public void sendProductActiveNotification(String businessPartnerCode, CategoryChangeMailEvent categoryChangeMailEvent,
      String productSku, boolean isBundleProduct) throws Exception {
    String message =
        String.format(PRODUCT_ACTIVE_CATEGORY_CHANGE, productSku, categoryChangeMailEvent.getOldCategoryMargin(),
            categoryChangeMailEvent.getNewCategoryMargin());
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        ConverterUtil.getNotificationDetailForProductActivation(productSku, isBundleProduct), message,
        NotificationType.PRODUCT_ACTIVATED_WITH_CATEGORY_CHANGE.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(), businessPartnerCode,
        notificationKafka);
  }

  @Override
  public void sendProductActiveNotification(String businessPartnerCode, String gdnProductSku, String productName,
      boolean isBundleProduct)
      throws Exception {
    String message = String.format(PRODUCT_ACTIVE, productName, gdnProductSku);
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        ConverterUtil.getNotificationDetailForProductActivation(gdnProductSku, isBundleProduct), message,
        NotificationType.PRODUCT_ACTIVATED.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(), businessPartnerCode,
        notificationKafka);
  }

  @Override
  public void sendProductQRCodeDownloadNotification(String businessPartnerCode, String filepath) throws Exception {
    String message = String.format(QR_CODE_DOWNLOAD, getFileName(filepath));
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        applicationProperties.getQRCodeUrlSource() + filepath,
        message, NotificationType.QRCODE_DOWNLOAD.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  public void sendProductSyncNotification(String businessPartnerCode, String message)
    throws Exception {
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka("/external/product/active/all",
        message, NotificationType.PRODUCT_SYNC_ACTIVITY.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  @Async
  @Trace(dispatcher=true)
  public void sendNotificationForProductWithRestrictedKeyword(String businessPartnerCode, String productName,
      boolean isInternational) throws Exception {
    String message =
        productName + StringUtils.SPACE + (isInternational ? RESTRICTED_KEY_MESSAGE_EN : RESTRICTED_KEY_MESSAGE_IN);
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(productName,
        message, NotificationType.RESTRICTED_PRODUCT_ACTIVATED.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  @Override
  @Async
  @Trace(dispatcher=true)
  public void sendNotificationForProductStatus(String businessPartnerCode, String productName, String status)
      throws Exception {
    String productStatus = null;
    if (ProductLevel3Status.TEASER.name().equals(status)) {
      productStatus = UPCOMING_STATUS;
    } else if (ProductLevel3Status.B2B.name().equals(status)) {
      productStatus = BUYABLE_LINK_STATUS;
    }
    String message = String.format(PRODUCT_STATUS, productName, productStatus);
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(productName,
        message, NotificationType.PRODUCT_STATUS.getValue(), businessPartnerCode);
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaProducer.send(applicationProperties.getWebNotifNotificationCreateTopic(),
        businessPartnerCode, notificationKafka);
  }

  private String getFileName(String path) {
    String folders[] = path.split(DELIMETER);
    return folders[folders.length - 1];
  }

  private ProductStockAlertNotification buildProductStockAlertModel(String businessPartnerCode,
      int productCount) {
    return ProductStockAlertNotification.builder().businessPartnerCode(businessPartnerCode)
        .count(productCount).build();
  }

  private void setMandatoryParamsToNotificationKafka(NotificationKafka notificationKafka) {
    notificationKafka.setStoreId(mandatoryParameterHelper.getStoreId());
    notificationKafka.setRequestId(mandatoryParameterHelper.getRequestId());
    notificationKafka.setChannelId(mandatoryParameterHelper.getChannelId());
    notificationKafka.setUsername(mandatoryParameterHelper.getUsername());
    notificationKafka.setClientId(mandatoryParameterHelper.getClientId());
    if(StringUtils.isBlank(notificationKafka.getStoreId())) {
      notificationKafka.setStoreId(Constants.DEFAULT_STORE_ID);
    }
  }
}
