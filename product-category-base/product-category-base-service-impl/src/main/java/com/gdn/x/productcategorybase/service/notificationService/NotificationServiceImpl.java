package com.gdn.x.productcategorybase.service.notificationService;

import java.nio.charset.StandardCharsets;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.util.ConverterUtil;

@Service
public class NotificationServiceImpl implements NotificationService {

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${wn.kafka.create-notification-topic}")
  private String webNotifNotificationCreateTopic;

  @Override
  public void createWebNotification(BrandWip brandWip) throws Exception {
    String message;
    if (BrandWipState.APPROVED.equals(brandWip.getState())) {
      message = new StringBuilder(Constants.BRAND_NOTIFICATION).append(brandWip.getBrandName())
              .append(Constants.BRAND_APPROVED_NOTIFICATION)
              .toString();
    } else {
      String notificationMsg =
          new StringBuilder(Constants.BRAND_NOTIFICATION).append(brandWip.getBrandName())
              .append(Constants.BRAND_REJECTED_NOTIFICATION)
              .append(new String(brandWip.getNotes(), StandardCharsets.UTF_8)).toString();
      message = StringUtils.abbreviate(notificationMsg, Constants.MAX_CHARACTER_FOR_NOTIFICATION);
    }
    NotificationKafka notificationKafka = ConverterUtil.getNotificationKafka(
        null,
        message, NotificationType.BRAND_APPROVAL.getValue(), brandWip.getBusinessPartnerCode());
    setMandatoryParamsToNotificationKafka(notificationKafka);
    kafkaPublisher.send(webNotifNotificationCreateTopic, brandWip.getBusinessPartnerCode(), notificationKafka);
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
