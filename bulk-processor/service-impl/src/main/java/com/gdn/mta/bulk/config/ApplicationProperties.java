package com.gdn.mta.bulk.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@Component
public class ApplicationProperties {

	@Value("${application.max.data.order}")
	private int maxDataOrder;

	@Value("${wn.kafka.create-notification-topic}")
	private String webNotifNotificationCreateTopic;

	@Value("${generate-qr-code-push-notification-topic}")
	private String generateQrCodePushNotificationTopic;

	@Value("${failure-qr-code-push-notification-topic}")
	private String failureQrCodePushNotificationTopic;

}
