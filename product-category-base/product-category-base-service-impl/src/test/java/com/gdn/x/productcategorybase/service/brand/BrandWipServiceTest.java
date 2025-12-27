package com.gdn.x.productcategorybase.service.brand;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.x.productcategorybase.dto.BrandCreationDTO;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.notification.dto.NotificationSummaryRequest;
import com.gdn.mta.notification.enumeration.NotificationType;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandServiceWrapperResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.repository.AttributeRepository;
import com.gdn.x.productcategorybase.repository.PredefinedAllowedAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandWipRepository;
import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.BusinessPartner.BusinessPartnerService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.SequenceService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;

public class BrandWipServiceTest {

  private static final String ID = "id";
  private static final String DEFAULT_BRAND_CODE = "BRD-00000";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_ID = "ID";
  private static final String DEFAULT_DESCRIPTION_STRING = "description";
  private static final byte[] DEFAULT_DESCRIPTION = DEFAULT_DESCRIPTION_STRING.getBytes();
  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_USERNAME = "username";
  private static final String DEFAULT_PREFIX = "BR";
  private static final Long DEFAULT_SEQUENCE = Long.valueOf(0);
  private static final String suffix = "-00000";
  private static final String DEFAULT_PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "BR-00001-00001";
  private static String prefix;
  private static final String DEFAULT_ATTRIBUTE_CODE = "AT-0001";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String DEFAULT_ADDITIONAL_DESCRIPTION = null;
  private static final String EMPTY_STORE_ID = StringUtils.EMPTY;
  private static final String EMPTY_BRAND_REQUEST_CODE = StringUtils.EMPTY;
  private static final String DESCRIPTION = "desc";
  private static final String INTERNAL_USER = "INTERNAL";
  private static final String BRAND = "Brand";
  private static final String NA = "NA";
  private static final String BRAND_HISTORY_DESCRIPTION = "brandDescription";
  private static final String HISTORY_NOTES_VALUE = "o: desc, n: NA";
  private static final String EMAIL_TO = "email@g.com";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.";
  private static final String DEFAULT_PROFILE_BANNER_PATH = "blibli-com-banner.";
  private static final String BRAND_LOGO = "brandLogo";
  private static final String PROFILE_BANNER = "profileBanner";
  private static final String BRAND_LOGO_NEW_PATH = "n: blibli-com-logo.";
  private static final String PROFILE_BANNER_NEW_PATH = "n: blibli-com-banner.";
  private static final String DRAFT = "DRAFT";

  private BrandWipSummaryRequest brandWipSummaryRequest = new BrandWipSummaryRequest();
  private BrandApproveRequest brandApproveRequest;
  private Brand brand1;
  private BrandWip brandWip2;
  private BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel;
  private Page<Attribute> attributePage;
  private Attribute attribute1;
  private SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel;
  private SolrUpdateBrandModel solrUpdateBrandModel;
  private ProfileResponse profileResponse;
  private BrandRejectRequest brandRejectRequest;
  private NotificationSummaryRequest notificationSummaryRequest;
  Map<String, String> brandDiffs;

  private static BrandWip brandWip = new BrandWip();
  private static BrandWipHistory brandWipHistory = new BrandWipHistory();
  private static SimpleDateFormat PREFIX_DATE_FORMATTER = new SimpleDateFormat("yyyyMMdd");
  private static Date date = Calendar.getInstance().getTime();
  private static PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
  private static Attribute attribute = new Attribute();
  private static Pageable pageable;
  private static List<Attribute> attributeList = new ArrayList<>();
  private static List<Attribute> emptyList = new ArrayList<>();
  private Page<BrandWip> brandWipPage;
  private BrandWip emptyBrandWip = new BrandWip();
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private BrandWipHistoryResponse brandWipHistoryResponse = new BrandWipHistoryResponse();
  private List<BrandWipHistoryResponse> brandWipHistoryResponseList = new ArrayList<>();
  private Page<BrandWipHistoryResponse> page;
  private BrandInReviewResponse brandInReviewResponse;

  @Mock
  private BrandWipRepository brandWipRepository;

  @Mock
  private BrandWipHistoryService brandWipHistoryService;

  @Mock
  private SequenceService sequenceService;

  @Mock
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Mock
  private AttributeService attributeService;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private BrandWipServiceBean brandWipServiceBean;

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Captor
  private ArgumentCaptor<List<SolrBrandModel>> solrBrandModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<Brand> brandArgumentCaptor;

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private AttributeRepository attributeRepository;

  @Mock
  private BusinessPartnerService businessPartnerService;


  @Mock
  private BrandService brandServiceBean;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private SequenceRepository sequenceRepository;

  @Mock
  private PredefinedAllowedAttributeValueRepository predefinedAllowedAttributeValueRepository;

  @Mock
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Mock
  private BrandAuthorisationService brandAuthorisationService;

  @Captor
  private ArgumentCaptor<SolrAddBrandListDomainEventModel> solrAddBrandDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<BrandWip> brandWipArgumentCaptor;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put("storeId",DEFAULT_STORE_ID);
    MDC.put("username", DEFAULT_USERNAME);
    prefix = DEFAULT_PREFIX + "-" + PREFIX_DATE_FORMATTER.format(date);
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    brandWip.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    brandWip.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    brandWip.setUpdatedBy(DEFAULT_USERNAME);
    brandWip.setBrandDescription(DEFAULT_DESCRIPTION);
    brandWip.setState(BrandWipState.DRAFT);
    brandWipHistory.setBrandRequestCode(prefix+suffix);
    brandWipHistory.setState(BrandWipState.DRAFT);
    brandWipHistory.setDescription(BrandWipState.DRAFT.getDescription().getBytes());
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(DEFAULT_PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    attribute.setAttributeCode(DEFAULT_ATTRIBUTE_CODE);
    pageable = PageRequest.of(PAGE, SIZE);
    attributeList.add(attribute);

    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_DESCRIPTION_STRING);
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipHistoryResponse.setState(BrandWipState.DRAFT.getDescription());
    brandWipHistoryResponseList.add(brandWipHistoryResponse);
    page = new PageImpl<>(brandWipHistoryResponseList);
    brandWipSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandWipSummaryRequest.setState("DRAFT");
    List<BrandWip> brandWips = new ArrayList<>();
    brandWips.add(brandWip);
    brandWipPage = new PageImpl<>(brandWips);
    brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandApproveRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandApproveRequest.setBrandDescription(DESCRIPTION);

    brandWip2 = new BrandWip();
    brandWip2.setId(ID);
    brandWip2.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip2.setBrandName(DEFAULT_BRAND_NAME);
    brandWip2.setBrandDescription(DESCRIPTION.getBytes());
    brandWip2.setState(BrandWipState.DRAFT);
    brandWip2.setBusinessPartnerCode(INTERNAL_USER);

    notificationSummaryRequest = new NotificationSummaryRequest();
    notificationSummaryRequest.setStoreId(DEFAULT_STORE_ID);
    notificationSummaryRequest.setDestinationKey(DEFAULT_BUSINESS_PARTNER_CODE);
    notificationSummaryRequest.setNotificationType(NotificationType.BRAND_APPROVAL.getValue());

    brand1 = new Brand();
    brand1.setBrandCode(DEFAULT_BRAND_CODE);
    brand1.setBrandName(DEFAULT_BRAND_NAME);
    brand1.setBrandDescription(DESCRIPTION.getBytes());
    brand1.setBrandWipId(ID);

    brandApprovedOrRejectedDomainEventModel = new BrandApprovedOrRejectedDomainEventModel();
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BrandWipState.APPROVED.name());
    brandApprovedOrRejectedDomainEventModel.setBrandCode(DEFAULT_BRAND_CODE);
    brandApprovedOrRejectedDomainEventModel.setBrandName(DEFAULT_BRAND_NAME);
    brandApprovedOrRejectedDomainEventModel.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    attribute1 = new Attribute();
    attribute1.setId(ID);
    attributePage = new PageImpl<>(Collections.singletonList(attribute1));

    solrUpdateBrandDomainEventModel = new SolrUpdateBrandDomainEventModel();
    solrUpdateBrandModel = new SolrUpdateBrandModel();
    solrUpdateBrandModel.setId(ID);
    solrUpdateBrandModel.setBrandCode(DEFAULT_BRAND_CODE);
    solrUpdateBrandModel.setBusinessPartnerCode(NA);
    solrUpdateBrandModel.setBrandApproved(true);

    profileResponse = new ProfileResponse();
    ResponsiblePersonDTO responsiblePersonDTO = new ResponsiblePersonDTO();
    responsiblePersonDTO.setEmail(EMAIL_TO);
    profileResponse.setResponsiblePerson(responsiblePersonDTO);

    brandRejectRequest = new BrandRejectRequest();
    brandRejectRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_DESCRIPTION_STRING);
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandWipHistoryResponse.setState(BrandWipState.DRAFT.getDescription());
    brandWipHistoryResponseList.add(brandWipHistoryResponse);
    page = new PageImpl<>(brandWipHistoryResponseList);
    brandWipSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandWipSummaryRequest.setState("DRAFT");
    brandWips.add(brandWip);
    brandWipPage = new PageImpl<>(brandWips);

    brandDiffs = new HashMap<>();
    brandDiffs.put(BRAND_LOGO, BRAND_LOGO_NEW_PATH);
    brandDiffs.put(PROFILE_BANNER, PROFILE_BANNER_NEW_PATH);

    brandInReviewResponse = new BrandInReviewResponse();
    brandInReviewResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandInReviewResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandInReviewResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.brandWipRepository, solrBrandRepository);
    Mockito.verifyNoMoreInteractions(this.sequenceService);
    Mockito.verifyNoMoreInteractions(this.brandWipHistoryService);
    Mockito.verifyNoMoreInteractions(this.predefinedAllowedAttributeValueService);
    Mockito.verifyNoMoreInteractions(this.brandRepository);
    Mockito.verifyNoMoreInteractions(this.sequenceRepository);
    Mockito.verifyNoMoreInteractions(this.attributeRepository);
    Mockito.verifyNoMoreInteractions(this.predefinedAllowedAttributeValueRepository);
    Mockito.verifyNoMoreInteractions(this.domainEventPublisherService);
    Mockito.verifyNoMoreInteractions(this.objectMapper);
    Mockito.verifyNoMoreInteractions(this.businessPartnerService);
    Mockito.verifyNoMoreInteractions(this.brandServiceBean);
    Mockito.verifyNoMoreInteractions(this.applicationCacheServiceBean);
    Mockito.verifyNoMoreInteractions(this.brandAuthorisationService);
  }

  @Test
  public void create_withBusinessPartner() throws Exception {
    brandWip.setProtectedBrand(false);
    Mockito.when(this.sequenceService.findByCode(prefix)).thenReturn(DEFAULT_SEQUENCE);
    Mockito.when(this.brandWipRepository.saveAndFlush(brandWip)).thenReturn(brandWip);
    Mockito
        .when(this.predefinedAllowedAttributeValueService.generatePredefinedAllowedAttributeValue(brandWip, attribute))
        .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(this.attributeService.findByName(anyString(), anyString())).thenReturn(attributeList);
    BrandCreationDTO brandCreationDTO = this.brandWipServiceBean.create(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(this.sequenceService).findByCode(prefix);
    Mockito.verify(this.brandWipRepository).saveAndFlush(brandWip);
    Mockito.verify(this.predefinedAllowedAttributeValueService)
        .generatePredefinedAllowedAttributeValue(brandWip, attribute);
    Mockito.verify(this.predefinedAllowedAttributeValueService).save(predefinedAllowedAttributeValue);
    Mockito.verify(this.attributeService)
        .findByName(anyString(), anyString());
    assertEquals(prefix + suffix, brandCreationDTO.getBrandWip().getBrandRequestCode());
  }

  @Test
  public void create_emptyAttribute() throws Exception {
    Mockito.when(this.sequenceService.findByCode(prefix)).thenReturn(DEFAULT_SEQUENCE);
    doNothing().when(this.brandWipHistoryService)
        .generateBrandWipHistory(brandWip, DEFAULT_ADDITIONAL_DESCRIPTION, DEFAULT_USERNAME);
    Mockito.when(this.brandWipRepository.save(brandWip)).thenReturn(brandWip);
    Mockito
        .when(this.predefinedAllowedAttributeValueService.generatePredefinedAllowedAttributeValue(brandWip, attribute))
        .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(this.attributeService.findByName(anyString(), anyString())).thenReturn(emptyList);
    try{
      this.brandWipServiceBean.create(DEFAULT_STORE_ID, brandWip);
    }catch (ApplicationRuntimeException r){

    }
    finally {
      Mockito.verify(this.attributeService)
          .findByName(anyString(), anyString());
      Mockito.verify(this.sequenceService).findByCode(anyString());
    }
  }

  @Test
  public void create_emptyStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandWipServiceBean.create(EMPTY_STORE_ID, brandWip));
  }

  @Test
  public void create_withEmptyBusinessPartnerCode() throws Exception {
    brandWip.setBusinessPartnerCode(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandWipServiceBean.create(EMPTY_STORE_ID, brandWip));
  }

  @Test
  public void create_withEmptyBusinessPartnerName() throws Exception {
    brandWip.setBusinessPartnerName(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandWipServiceBean.create(EMPTY_STORE_ID, brandWip));
  }

  @Test
  public void getBrandWipDetailTest() throws Exception {
    brandWip.setValidBrand(true);
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWip);
    BrandWipResponse brandWipResponse =
        this.brandWipServiceBean.getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipRepository)
        .findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Assertions.assertTrue(brandWipResponse.isValidBrand());
  }

  @Test
  public void getBrandWipDetail_emptyStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.brandWipServiceBean.getBrandWipDetail(EMPTY_STORE_ID, DEFAULT_BRAND_REQUEST_CODE));
  }

  @Test
  public void getBrandWipDetail_emptyBrandRequestCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.brandWipServiceBean.getBrandWipDetail(DEFAULT_STORE_ID, EMPTY_BRAND_REQUEST_CODE));
  }

  @Test
  public void getBrandRejectionInfoResponseTest() throws Exception {
    brandWip.setId(DEFAULT_ID);
    brandWip.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWip.setNotes(DEFAULT_DESCRIPTION);
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    Mockito.when(this.brandWipRepository
        .findByStoreIdAndBrandRequestCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, BrandWipState.REJECTED))
        .thenReturn(brandWip);
    BrandRejectionInfoResponse brandRejectionInfoResponse =
        this.brandWipServiceBean.getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipRepository)
        .findByStoreIdAndBrandRequestCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, BrandWipState.REJECTED);
    assertEquals(DEFAULT_DESCRIPTION_STRING, brandRejectionInfoResponse.getRejectionReason());
    assertEquals(DEFAULT_BRAND_NAME, brandRejectionInfoResponse.getBrandName());
    assertEquals(DEFAULT_ID, brandRejectionInfoResponse.getId());
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, brandRejectionInfoResponse.getBrandRequestCode());
  }

  @Test
  public void getBrandRejectionInfoResponseNullTest() throws Exception {
    Mockito.when(this.brandWipRepository
        .findByStoreIdAndBrandRequestCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE, BrandWipState.REJECTED))
        .thenReturn(null);
    try {
      this.brandWipServiceBean.getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(this.brandWipRepository)
          .findByStoreIdAndBrandRequestCodeAndState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE,
              BrandWipState.REJECTED);
    }
  }

  @Test
  public void getBrandWipListTest() throws  Exception {
    Mockito.when(this.brandWipRepository
        .findByBrandNameAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.DRAFT, pageable))
        .thenReturn(brandWipPage);
    Page<BrandWipResponse> brandWipResponsePage =
        this.brandWipServiceBean.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    Mockito.verify(this.brandWipRepository)
        .findByBrandNameAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.DRAFT, pageable);
    assertEquals(DEFAULT_BRAND_NAME, brandWipResponsePage.getContent().get(0).getBrandName());
    assertEquals(DEFAULT_DESCRIPTION_STRING, brandWipResponsePage.getContent().get(0).getBrandDescription());
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, brandWipResponsePage.getContent().get(0).getBrandRequestCode());
  }

  @Test
  public void getBrandWipListEmptyTest() throws  Exception {
    Mockito.when(this.brandWipRepository
        .findByBrandNameAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.DRAFT, pageable))
        .thenReturn(null);
    Exception exception = null;
    try {
      this.brandWipServiceBean.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    } catch (Exception e) {
      exception = e;
    } finally {
      assertEquals(ApplicationRuntimeException.class, exception.getClass());
      Mockito.verify(this.brandWipRepository)
          .findByBrandNameAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.DRAFT, pageable);
    }
  }

  @Test
  public void getBrandWipList_emptyBrandNameTest() throws Exception {
    brandWipSummaryRequest.setBrandName(StringUtils.EMPTY);
    Mockito.when(this.brandWipRepository.findByStateAndMarkForDeleteFalse(BrandWipState.DRAFT, pageable))
        .thenReturn(brandWipPage);
    Page<BrandWipResponse> brandWipResponsePage =
        this.brandWipServiceBean.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
    Mockito.verify(this.brandWipRepository).findByStateAndMarkForDeleteFalse(BrandWipState.DRAFT, pageable);
    assertEquals(DEFAULT_BRAND_NAME, brandWipResponsePage.getContent().get(0).getBrandName());
    assertEquals(DEFAULT_DESCRIPTION_STRING, brandWipResponsePage.getContent().get(0).getBrandDescription());
  }

  @Test
  public void getBrandWipDetail_emptyResponse() throws Exception {
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(null);
    try {
      this.brandWipServiceBean.getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    } catch (ApplicationRuntimeException e) {

    } finally {
      Mockito.verify(this.brandWipRepository)
          .findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    }
  }

  @Test
  public void deleteBrandWipTest() {
    brandWipServiceBean.deleteBrandWip(brandWip);
    Mockito.verify(brandWipRepository).save(brandWipArgumentCaptor.capture());
    assertEquals(BrandWipState.DELETED, brandWipArgumentCaptor.getValue().getState());
    Assertions.assertTrue(brandWipArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void getBrandWipByStoreIdAndBrandCodeTest() {
    brandWipServiceBean.getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).findByStoreIdAndBrandCodeAndMarkForDeleteFalse(
        DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandWipByStoreIdAndBrandCodeEmptyBrandCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> brandWipServiceBean.getBrandWipByStoreIdAndBrandCode(DEFAULT_STORE_ID, null));
  }

  @Test
  public void getBrandWipHistoryTest_withBrandRequestCode() throws Exception {
    Mockito.when(this.brandWipHistoryService
        .findByBrandRequestCode(brandWipHistorySummaryRequest.getBrandRequestCode(), pageable)).thenReturn(page);
    Page<BrandWipHistoryResponse> responses = this.brandWipServiceBean.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    Mockito.verify(this.brandWipHistoryService).
    findByBrandRequestCode(brandWipHistorySummaryRequest.getBrandRequestCode(), pageable);
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, responses.getContent().get(0).getBrandRequestCode());
    assertEquals(DEFAULT_DESCRIPTION_STRING, responses.getContent().get(0).getDescription());
  }

  @Test
  public void getBrandWipHistoryTest_withBrandCode() throws Exception {
    brandWipHistorySummaryRequest.setBrandRequestCode(StringUtils.EMPTY);
    brandWipHistorySummaryRequest.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.when(this.brandWipHistoryService
        .findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable)).thenReturn(page);
    Page<BrandWipHistoryResponse> responses = this.brandWipServiceBean.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    Mockito.verify(this.brandWipHistoryService).
        findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable);
    assertEquals(DEFAULT_BRAND_CODE, responses.getContent().get(0).getBrandCode());
    assertEquals(DEFAULT_DESCRIPTION_STRING, responses.getContent().get(0).getDescription());
  }

  @Test
  public void getBrandWipHistoryTest_emptyBrandWipHistorySummaryRequest() throws Exception {
    brandWipHistorySummaryRequest.setBrandRequestCode(StringUtils.EMPTY);
    brandWipHistorySummaryRequest.setBrandCode(BRAND);
    Mockito.when(this.brandWipHistoryService
        .findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable)).thenReturn(page);
    this.brandWipServiceBean.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
    Mockito.verify(this.brandWipHistoryService).
        findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable);
  }

  @Test
  public void getBrandWipHistoryTest_emptyResponse() throws Exception {
    brandWipHistorySummaryRequest.setBrandRequestCode(StringUtils.EMPTY);
    brandWipHistorySummaryRequest.setBrandCode(BRAND);
    Mockito.when(this.brandWipHistoryService
        .findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable)).thenReturn(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.brandWipServiceBean.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE));
    Mockito.verify(this.brandWipHistoryService).
        findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable);
  }

  @Test
  public void updateTest() throws Exception {
    brandWip.setStoreId(DEFAULT_STORE_ID);
    brandWip.setBusinessPartnerName(null);
    brandWip.setBusinessPartnerCode(null);
    brandWip.setBrandDescription(DESCRIPTION.getBytes());
    brandWip.setState(BrandWipState.APPROVED);
    brandWip.setProtectedBrand(true);
    brandWip.setSkuCreationAllowedForAllSellers(true);
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWip);
    doNothing().when(this.brandWipHistoryService).generateBrandWipHistory(brandWip, DESCRIPTION, DEFAULT_USERNAME);
    Mockito.when(this.brandWipRepository.save(brandWip)).thenReturn(brandWip);
    Mockito.when(this.objectMapper.writeValueAsString(Mockito.any())).thenReturn(DESCRIPTION);
    this.brandWipServiceBean.update(DEFAULT_STORE_ID, brandApproveRequest);
    Mockito.verify(this.objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(this.brandWipRepository).findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipHistoryService, times(1))
        .generateBrandWipHistory(brandWip, DESCRIPTION, DEFAULT_USERNAME);
    Mockito.verify(this.brandWipRepository).save(brandWip);
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(brandWip.getBrandRequestCode());
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(brandWip.getStoreId());
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
  }

  @Test
  public void updateDifferentValidBrandFlagTest() throws Exception {
    brandWip.setBusinessPartnerName(null);
    brandWip.setBusinessPartnerCode(null);
    brandWip.setBrandDescription(DESCRIPTION.getBytes());
    brandWip.setState(BrandWipState.DRAFT);
    brandApproveRequest.setValidBrand(true);
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWip);
    doNothing().when(this.brandWipHistoryService).generateBrandWipHistory(brandWip, DESCRIPTION, DEFAULT_USERNAME);
    Mockito.when(this.brandWipRepository.save(brandWipArgumentCaptor.capture())).thenReturn(brandWip);
    Mockito.when(this.objectMapper.writeValueAsString(Mockito.any())).thenReturn(DESCRIPTION);
    this.brandWipServiceBean.update(DEFAULT_STORE_ID, brandApproveRequest);
    Mockito.verify(this.objectMapper).writeValueAsString(Mockito.any());
    Mockito.verify(this.brandWipRepository).findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipHistoryService, times(1))
        .generateBrandWipHistory(brandWip, DESCRIPTION, DEFAULT_USERNAME);
    Mockito.verify(this.brandWipRepository).save(brandWipArgumentCaptor.getValue());
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
    Assertions.assertTrue(brandWipArgumentCaptor.getValue().isValidBrand());
  }

  @Test
  public void updateTest_expectApplicationRuntimeException() throws Exception {
    brandWip.setBusinessPartnerName(null);
    brandWip.setBusinessPartnerCode(null);
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(null);
    try {
      this.brandWipServiceBean.update(DEFAULT_STORE_ID, brandApproveRequest);
    } catch (ApplicationRuntimeException e) {

    } finally {
      Mockito.verify(this.brandWipRepository)
          .findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    }
  }

  @Test
  public void approveBrandUnDeleteBrandTest() throws Exception {
    brandWip2.setStoreId(DEFAULT_STORE_ID);
    brandApproveRequest.setValidBrand(true);
    brandApproveRequest.setProtectedBrand(true);
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(INTERNAL_USER);
    brandApprovedOrRejectedDomainEventModel.setProtectedBrand(true);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    Mockito.when(
        brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true))
        .thenReturn(brand1);
    Mockito.when(attributeRepository
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, BRAND, PageRequest.of(0, 10)))
        .thenReturn(attributePage);
    Mockito.when(brandServiceBean.undelete(brandArgumentCaptor.capture())).thenReturn(brand1);
    BrandServiceWrapperResponse response = brandWipServiceBean.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipRepository)
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true);
    brandWip2.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService).generateBrandWipHistory(brandWip2, null, DEFAULT_USERNAME);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.anyMap());
    Mockito.verify(brandServiceBean).undelete(Mockito.any(Brand.class));
    Mockito.verify(this.applicationCacheServiceBean).evictBrandAuthorizationCache(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.applicationCacheServiceBean).evictProtectedBrandCache(DEFAULT_STORE_ID);
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
    assertEquals(DEFAULT_BRAND_CODE, response.getBrandWip().getBrandCode());
    Assertions.assertTrue(brandArgumentCaptor.getValue().isValidBrand());
  }

  @Test
  public void approveBrandUnDeleteBrandNewImageTest() throws Exception {
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setProfileBannerPath(DEFAULT_PROFILE_BANNER_PATH);
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(INTERNAL_USER);
    brandWip2.setStoreId(DEFAULT_STORE_ID);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    Mockito.when(
        brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true))
        .thenReturn(brand1);
    Mockito.when(attributeRepository
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, BRAND, PageRequest.of(0, 10)))
        .thenReturn(attributePage);
    Mockito.when(brandServiceBean.undelete(Mockito.any(Brand.class))).thenReturn(brand1);
    BrandServiceWrapperResponse response = brandWipServiceBean.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipRepository)
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true);
    brandWip2.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService).generateBrandWipHistory(brandWip2, null, DEFAULT_USERNAME);
    Mockito.verify(objectMapper).writeValueAsString(brandDiffs);
    Mockito.verify(brandServiceBean).undelete(Mockito.any(Brand.class));
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
    assertEquals(DEFAULT_BRAND_CODE, response.getBrandWip().getBrandCode());
    assertEquals(DEFAULT_BRAND_LOGO_PATH, response.getBrandWip().getBrandLogoPath());
    assertEquals(DEFAULT_PROFILE_BANNER_PATH, response.getBrandWip().getProfileBannerPath());
  }

  @Test
  public void approveNewBrandTest() throws Exception {
    brandApproveRequest.setValidBrand(true);
    Brand brand = new Brand();
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip2.setBusinessPartnerCode(null);
    brandWip2.setStoreId(DEFAULT_STORE_ID);
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(null);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    Mockito.when(
        brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true))
        .thenReturn(null);
    Mockito.when(brandRepository
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, false))
        .thenReturn(null);
    Mockito.when(attributeRepository
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, BRAND, PageRequest.of(0, 10)))
        .thenReturn(attributePage);
    BrandServiceWrapperResponse response = brandWipServiceBean.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipRepository)
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, false);
    Mockito.verify(brandRepository).save(brandArgumentCaptor.capture());
    Mockito.verify(predefinedAllowedAttributeValueService)
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(eq(DEFAULT_STORE_ID), eq(DEFAULT_BRAND_REQUEST_CODE),
            brandArgumentCaptor.capture());
    Mockito.verify(this.sequenceRepository).findByCode(anyString());
    Mockito.verify(domainEventPublisherService).publishBrandUpdated(Mockito.any(Brand.class));
    brandWip2.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService).generateBrandWipHistory(brandWip2, null, DEFAULT_USERNAME);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.anyMap());
    Mockito.verify(businessPartnerService).createBrandStatusChangeNotification(brandWip2);
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
    assertEquals(DEFAULT_BRAND_CODE, response.getBrandWip().getBrandCode());
    Assertions.assertTrue(brandArgumentCaptor.getValue().isValidBrand());
  }

  @Test
  public void approveBrandExceptionTest() throws Exception {
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(null);
    try {
      brandWipServiceBean.approveBrand(brandApproveRequest);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(brandWipRepository)
          .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
      Mockito.verify(brandWipRepository)
          .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
      Mockito.verify(brandWipRepository)
          .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
              brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    }
  }

  @Test
  public void approveNewBrandHistoryChangesTest() throws Exception {
    Brand brand = new Brand();
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brandWip2.setStoreId(DEFAULT_STORE_ID);
    brandApproveRequest.setBrandDescription(NA);
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(INTERNAL_USER);
    Map<String, String> brandDiffs = new HashMap<>();
    brandDiffs.put(BRAND_HISTORY_DESCRIPTION, HISTORY_NOTES_VALUE);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    Mockito.when(
        brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true))
        .thenReturn(null);
    Mockito.when(brandRepository
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, false))
        .thenReturn(null);
    Mockito.when(attributeRepository
        .findByStoreIdAndNameLikeIgnoreCaseAndMarkForDeleteFalse(DEFAULT_STORE_ID, BRAND, PageRequest.of(0, 10)))
        .thenReturn(attributePage);
    BrandServiceWrapperResponse response = brandWipServiceBean.approveBrand(brandApproveRequest);
    Mockito.verify(brandWipRepository)
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, true);
    Mockito.verify(brandRepository)
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME, false);
    Mockito.verify(brandRepository).save(Mockito.any(Brand.class));
    Mockito.verify(predefinedAllowedAttributeValueService)
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(eq(DEFAULT_STORE_ID), eq(DEFAULT_BRAND_REQUEST_CODE),
            brandArgumentCaptor.capture());
    Mockito.verify(this.sequenceRepository).findByCode(anyString());
    Mockito.verify(domainEventPublisherService).publishBrandUpdated(Mockito.any(Brand.class));
    brandWip2.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService).generateBrandWipHistory(brandWip2, null, DEFAULT_USERNAME);
    Mockito.verify(objectMapper).writeValueAsString(brandDiffs);
    Mockito.verify(brandAuthorisationService).updateBrandNameByBrandCode(anyString(), anyString(), anyString());
    assertEquals(DEFAULT_BRAND_CODE, response.getBrand().getBrandCode());
  }

  @Test
  public void approveAlreadyApprovedBrand() throws Exception {
    Mockito.when(brandWipRepository
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED))
        .thenReturn(brandWip2);
    try {
      brandWipServiceBean.approveBrand(brandApproveRequest);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(brandWipRepository)
          .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
      Mockito.verify(brandWipRepository)
          .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME, BrandWipState.APPROVED);
    }
  }
  @Test
  public void approveExistingApprovedBrand() throws Exception {
    Mockito.when(brandWipRepository
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED))
        .thenReturn(brandWip2);
    try {
      brandWipServiceBean.approveBrand(brandApproveRequest);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(brandWipRepository)
          .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE, BrandWipState.APPROVED);
    }
  }

  @Test
  public void rejectBrandTest() throws Exception {
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BrandWipState.REJECTED.name());
    brandApprovedOrRejectedDomainEventModel.setBrandCode(null);
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(INTERNAL_USER);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    brandWipServiceBean.rejectBrand(brandRejectRequest);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .updatePredefinedAllowedAttributeCodeForRejectedBrand(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService).generateBrandWipHistory(brandWip2, null, DEFAULT_USERNAME);
  }

  @Test
  public void rejectBrandNotesNonEmptyTest() throws Exception {
    brandApprovedOrRejectedDomainEventModel.setBrandApprovalStatus(BrandWipState.REJECTED.name());
    brandApprovedOrRejectedDomainEventModel.setBrandCode(null);
    brandRejectRequest.setNotes(NA);
    brandWip2.setNotes(NA.getBytes());
    brandApprovedOrRejectedDomainEventModel.setBusinessPartnerCode(INTERNAL_USER);
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(brandWip2);
    brandWipServiceBean.rejectBrand(brandRejectRequest);
    Mockito.verify(brandWipRepository)
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    Mockito.verify(predefinedAllowedAttributeValueService)
        .updatePredefinedAllowedAttributeCodeForRejectedBrand(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(brandWipRepository).save(brandWip2);
    Mockito.verify(domainEventPublisherService)
        .publishBrandApprovedOrRejectedDomainEventModel(brandApprovedOrRejectedDomainEventModel);
    Mockito.verify(brandWipHistoryService)
        .generateBrandWipHistory(brandWip2, brandRejectRequest.getNotes(), DEFAULT_USERNAME);
  }

  @Test
  public void rejectBrandExceptionTest() throws Exception {
    Mockito.when(brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
        brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT)).thenReturn(null);
    try {
      brandWipServiceBean.rejectBrand(brandRejectRequest);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(brandWipRepository)
          .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(DEFAULT_STORE_ID,
              brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    }
  }

  @Test
  public void getBrandWipDetailByBrandCodeTest() {
    brandWip.setBrandCode(DEFAULT_BRAND_CODE);
    Mockito
        .when(brandWipRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brandWip);
    BrandWipResponse response = brandWipServiceBean.getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    assertNotNull(response);
    assertEquals(DEFAULT_BRAND_CODE, response.getBrandCode());
    assertEquals(DEFAULT_DESCRIPTION_STRING, response.getBrandDescription());
  }

  @Test
  public void getBrandWipDetailByBrandCodeExceptionTest() {
    Mockito
        .when(brandWipRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(null);
    try {
      brandWipServiceBean.getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(brandWipRepository)
          .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    }
  }
  @Test
  public void findByBrandNameAndBusinessPartnerCodeTest() throws Exception {
    Mockito.when(this.brandWipRepository
        .findByBrandNameAndBusinessPartnerCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME,
            DEFAULT_BUSINESS_PARTNER_CODE, BrandWipState.DRAFT)).thenReturn(brandWip);
    BrandWipResponse response = this.brandWipServiceBean
        .findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.brandWipRepository)
        .findByBrandNameAndBusinessPartnerCodeAndStateAndMarkForDeleteFalse(DEFAULT_BRAND_NAME,
            DEFAULT_BUSINESS_PARTNER_CODE, BrandWipState.DRAFT);
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getBrandRequestCode());
    assertEquals(DEFAULT_DESCRIPTION_STRING, response.getBrandDescription());
    assertEquals(DEFAULT_BRAND_NAME, response.getBrandName());
  }

  @Test
  public void filterByBrandRequestCodeTest() throws Exception {
    Mockito.when(this.brandWipRepository.findByBrandRequestCodeAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE)).
        thenReturn(brandWip);
    BrandWipResponse response = this.brandWipServiceBean.filterByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipRepository).findByBrandRequestCodeAndMarkForDeleteFalse(DEFAULT_BRAND_REQUEST_CODE);
    assertEquals(DEFAULT_BRAND_NAME, response.getBrandName());
    assertEquals(DEFAULT_DESCRIPTION_STRING, response.getBrandDescription());
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getBrandRequestCode());
  }

  @Test
  public void filterByBrandRequestCodeIrrespectiveOfState() throws Exception {
    Mockito.when(this.brandWipRepository.findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE)).
        thenReturn(brandWip);
    BrandWipResponse response = this.brandWipServiceBean
        .filterByBrandRequestCodeIrrespectiveOfState(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(this.brandWipRepository)
        .findByStoreIdAndBrandRequestCode(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
    assertEquals(DEFAULT_BRAND_NAME, response.getBrandName());
    assertEquals(DEFAULT_DESCRIPTION_STRING, response.getBrandDescription());
    assertEquals(DEFAULT_BRAND_REQUEST_CODE, response.getBrandRequestCode());
  }

  @Test
  public void getBrandByNameFromBrandWipTest() {
    Mockito.when(this.brandWipRepository
        .findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME)).
        thenReturn(brandWip);
    BrandResponse response = this.brandWipServiceBean.getBrandByNameFromBrandWip(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(this.brandWipRepository)
        .findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    assertEquals(DEFAULT_BRAND_NAME, response.getBrandName());
    assertEquals(DEFAULT_DESCRIPTION_STRING, response.getBrandDescription());
  }

  @Test
  public void getBrandByNameFromBrandWipNullTest() {
    Mockito.when(this.brandWipRepository
        .findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME)).
        thenReturn(null);
    BrandResponse response = this.brandWipServiceBean.getBrandByNameFromBrandWip(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Mockito.verify(this.brandWipRepository)
        .findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(DEFAULT_STORE_ID, DEFAULT_BRAND_NAME);
    Assertions.assertNull(response);
  }

  @Test
  public void updateValidBrandFlagTest() {
    Mockito.doNothing().when(brandWipRepository)
        .updateValidBrandFlagByBrandCode(DEFAULT_USERNAME, true, DEFAULT_BRAND_CODE);
    brandWipServiceBean.updateValidBrandFlag(DEFAULT_USERNAME, DEFAULT_BRAND_CODE, true);
    verify(brandWipRepository).updateValidBrandFlagByBrandCode(DEFAULT_USERNAME, true, DEFAULT_BRAND_CODE);
  }

  @Test
  public void create_withProtectedBrand() throws Exception {
    brandWip.setProtectedBrand(true);
    Mockito.when(this.sequenceService.findByCode(prefix)).thenReturn(DEFAULT_SEQUENCE);
    doNothing().when(this.brandWipHistoryService)
      .generateBrandWipHistory(brandWip, DEFAULT_ADDITIONAL_DESCRIPTION, DEFAULT_USERNAME);
    Mockito.when(this.brandWipRepository.saveAndFlush(brandWip)).thenReturn(brandWip);
    Mockito
      .when(this.predefinedAllowedAttributeValueService.generatePredefinedAllowedAttributeValue(brandWip, attribute))
      .thenReturn(predefinedAllowedAttributeValue);
    Mockito.when(this.attributeService.findByName(anyString(), anyString())).thenReturn(attributeList);
    doNothing().when(this.attributeService)
      .evictAttributeCache(DEFAULT_STORE_ID, attribute.getId(), attribute.getAttributeCode());
    BrandCreationDTO brandCreationDTO = this.brandWipServiceBean.create(DEFAULT_STORE_ID, brandWip);
    Mockito.verify(this.sequenceService).findByCode(prefix);
    Mockito.verify(this.brandWipRepository).saveAndFlush(brandWip);
    Mockito.verify(this.predefinedAllowedAttributeValueService)
      .generatePredefinedAllowedAttributeValue(brandWip, attribute);
    Mockito.verify(this.predefinedAllowedAttributeValueService).save(predefinedAllowedAttributeValue);
    Mockito.verify(this.attributeService)
      .findByName(anyString(), anyString());
    assertEquals(prefix + suffix, brandCreationDTO.getBrandWip().getBrandRequestCode());

  }

  @Test
  public void getAllInReviewBrandsTest(){
    Mockito.when(brandWipRepository.findByStoreIdAndStateAndMarkForDeleteFalse(DEFAULT_STORE_ID,
      BrandWipState.DRAFT)).thenReturn(Arrays.asList(brandWip2));
    List<BrandInReviewResponse> result = brandWipServiceBean.getAllInReviewBrands(DEFAULT_STORE_ID);
  Mockito.verify(brandWipRepository).findByStoreIdAndStateAndMarkForDeleteFalse(DEFAULT_STORE_ID,
    BrandWipState.DRAFT);
  assertNotNull(result);
  }

  @Test
  public void updateBrandNameTest() {
    brandWip.setBrandName(DEFAULT_BRAND_NAME+"2");
    Mockito.when(brandWipRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID,
        DEFAULT_BRAND_CODE)).thenReturn(brandWip);
    brandWipServiceBean.updateBrandName(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_BRAND_NAME);
    Mockito.verify(brandWipRepository)
        .findByStoreIdAndBrandCodeAndMarkForDeleteFalse(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(brandWipRepository).save(brandWip);
  }
}

