package com.gdn.x.productcategorybase.service.businessPartner;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.notification.dto.NotificationSummaryRequest;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.outbound.xbp.feign.XbpFeign;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;
import com.gdn.x.productcategorybase.service.notificationService.NotificationService;

public class BusinessPartnerServiceImplTest {


  private static final String ID = "id";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DESCRIPTION = "desc";
  private static final String REJECTION_REASON =
      "Brand is rejected due to reason which should exceed 255 characters to test a bug.Brand is rejected due to reason"
          + " which should exceed 255 characters to test a bug. Brand is rejected due to reason"
          + "which should exceed 255 characters to test a bug. Brand is rejected due to reason which should exceed 255";
  private static final String EMAIL_TO = "email@g.com";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String MESSAGE_TEMPLATE_ID_BRAND_APPROVED = "brandApprovedMessageTemplateId";
  private static final String MESSAGE_TEMPLATE_ID_BRAND_REJECTED = "brandRejectedMessageTemplateId";
  private static final String MAIL_FROM = "no-reply@blibli.com";
  private static final String MESSAGE_IDENTIFIER_KEY = "brandRequestCode";
  private static final String BRAND_APPROVE_SUBJECT = "Blibli.com - Notifikasi Persetujuan Brand";
  private static final String BRAND_REJECT_SUBJECT = "Blibli.com - Notifikasi Penolakan Brand";
  private static final String BRAND_REJECT_NOTIFICATION = "Brand Blibli.com ditolak. Alasan : Brand is rejected due to "
      + "reason which should exceed 255 characters to test a bug.Brand is rejected due to reason which should exceed 255"
      + " characters to test a bug. Brand is rejected due to reasonwhich should exceed 255...";
  private static final String BRAND_APPROVE_NOTIFICATION = "Brand Blibli.com disetujui";

  private ProfileResponse profileResponse;
  private BrandWip brandWip2;
  private Map<String, Object> emailObjects;
  private NotificationSummaryRequest notificationSummaryRequest;
  private ResponsiblePersonDTO responsiblePerson;
  private GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse;

  @InjectMocks
  private BusinessPartnerServiceImpl businessPartnerService;

  @Mock
  private XbpFeign xbpFeign;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private NotificationService notificationService;

  @Captor
  private ArgumentCaptor<MailRecipientRequest> mailRecipientRequestArgumentCaptor;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    MDC.put("requestId", DEFAULT_REQUEST_ID);
    profileResponse = new ProfileResponse();
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);

    responsiblePerson = new ResponsiblePersonDTO();
    responsiblePerson.setEmail(EMAIL_TO);
    profileResponse.setResponsiblePerson(responsiblePerson);

    brandWip2 = new BrandWip();
    brandWip2.setId(ID);
    brandWip2.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip2.setBrandName(DEFAULT_BRAND_NAME);
    brandWip2.setBrandDescription(DESCRIPTION.getBytes());
    brandWip2.setState(BrandWipState.DRAFT);
    brandWip2.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);

    notificationSummaryRequest = new NotificationSummaryRequest();
    notificationSummaryRequest.setStoreId(DEFAULT_STORE_ID);
    notificationSummaryRequest.setDestinationKey(DEFAULT_BUSINESS_PARTNER_CODE);
    notificationSummaryRequest.setNotificationType(NotificationType.BRAND_APPROVAL.getValue());

    emailObjects = new HashMap<>();
    emailObjects.put("brandName", DEFAULT_BRAND_NAME);

    gdnRestSingleResponse = new GdnRestSingleResponse<ProfileResponse>(null, null, false, null, DEFAULT_REQUEST_ID);
  }

  @Test
  public void getBusinessPartnerProfileTest() throws Exception {
    Mockito.when(xbpFeign.getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID));
    ProfileResponse response = businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
  }

  @Test
  public void getBusinessPartnerProfileExceptionTest() throws Exception {
    Mockito.when(xbpFeign.getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE)))
        .thenReturn(gdnRestSingleResponse);
    try {
      businessPartnerService.getBusinessPartnerProfile(DEFAULT_BUSINESS_PARTNER_CODE);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(xbpFeign).getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
          Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
          Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
          Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
          Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    }
  }

  @Test
  public void createBrandStatusChangeNotificationRejectedTest() throws Exception {
    brandWip2.setNotes(REJECTION_REASON.getBytes());
    emailObjects.put("notes", REJECTION_REASON);
    notificationSummaryRequest.setNotificationMessage(BRAND_REJECT_NOTIFICATION);
    Mockito.doReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID)).when(xbpFeign)
        .getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    businessPartnerService.createBrandStatusChangeNotification(brandWip2);
    Mockito.verify(xbpFeign).getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(mailDeliveryService).sendMail(Mockito.eq(MESSAGE_TEMPLATE_ID_BRAND_REJECTED), Mockito.eq(MAIL_FROM),
        Mockito.eq(BRAND_REJECT_SUBJECT), Mockito.eq(emailObjects), Mockito.eq(MESSAGE_IDENTIFIER_KEY),
        Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), mailRecipientRequestArgumentCaptor.capture());
    Mockito.verify(notificationService).createWebNotification(brandWip2);
    Assertions.assertEquals(EMAIL_TO, mailRecipientRequestArgumentCaptor.getValue().getEmailTo());
  }

  @Test
  public void createBrandStatusChangeNotificationApprovedTest() throws Exception {
    brandWip2.setState(BrandWipState.APPROVED);
    notificationSummaryRequest.setNotificationMessage(BRAND_APPROVE_NOTIFICATION);
    Mockito.doReturn(new GdnRestSingleResponse<>(profileResponse, DEFAULT_REQUEST_ID)).when(xbpFeign)
        .getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    businessPartnerService.createBrandStatusChangeNotification(brandWip2);
    Mockito.verify(xbpFeign).getBusinessPartnerDetails(Mockito.eq(GdnMandatoryParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryParameterUtil.getUsername()), Mockito.eq(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.verify(mailDeliveryService).sendMail(Mockito.eq(MESSAGE_TEMPLATE_ID_BRAND_APPROVED), Mockito.eq(MAIL_FROM),
        Mockito.eq(BRAND_APPROVE_SUBJECT), Mockito.eq(emailObjects), Mockito.eq(MESSAGE_IDENTIFIER_KEY),
        Mockito.eq(DEFAULT_BRAND_REQUEST_CODE), mailRecipientRequestArgumentCaptor.capture());
    Mockito.verify(notificationService).createWebNotification(brandWip2);
    Assertions.assertEquals(EMAIL_TO, mailRecipientRequestArgumentCaptor.getValue().getEmailTo());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.mailDeliveryService);
    Mockito.verifyNoMoreInteractions(this.notificationService);
  }
}