package com.gdn.mta.product.service;

import static org.junit.jupiter.api.Assertions.assertEquals;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.MarginRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.FilterMarginsByOrderItemsRequest;
import com.gdn.mta.product.valueobject.Margin;
import com.gdn.mta.product.valueobject.OrderItemMarginsResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.mailEvent.CategoryChangeMailEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEvents;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3ItemWip;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;
import com.gdn.partners.pbp.outbound.margin.feign.MarginFeign;
import com.gdn.partners.pbp.repository.mailEvent.ProductMailEventsRepository;
import com.gdn.partners.pbp.repository.productlevel3.ProductLevel3WipRepository;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class ProductMailEventServiceImplTest {
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_CODE_1 = "PRODUCT_CODE_1";
  private static final String PRODUCT_ID = "PRODUCT_ID";
  private static final String PRODUCT_ITEM_NAME = "PRODUCT_ITEM_NAME";
  private static final String STATE = "STATE";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String PRODUCT_SKU_1 = "PRODUCT_SKU_1";
  private static final String NOTES = "NOTES";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String OLD_CATEGORY_CODE = "OLD_CATEGORY_CODE";
  private static final String NEW_CATEGORY_CODE = "NEW_CATEGORY_CODE";
  private static final String STORE_ID = "storeId";
  private static final String LANGUAGE_IN = "-in";
  private static final String LANGUAGE_EN = "-en";

  private ArgumentCaptor<ProductMailEvents> productMailEventsArgumentCaptor = ArgumentCaptor.forClass(ProductMailEvents.class);
  private ArgumentCaptor<ProductMailDomainEvent> productMailDomainEventArgumentCaptor = ArgumentCaptor.forClass(ProductMailDomainEvent.class);
  private List<String> ITEM_SKU_LIST = new ArrayList<>();
  private List<String> ITEM_NAME_LIST = new ArrayList<>();
  private List<List<String>> PRODUCT_ITEM_DATA = new ArrayList<>();
  private List<ProductMailEventsEnum> EVENTS = Arrays.asList(ProductMailEventsEnum.SUSPENDED, ProductMailEventsEnum.RE_ACTIVATED);
  private List<ProductMailEventsEnum> APPROVED_EVENTS =
      Arrays.asList(ProductMailEventsEnum.APPROVED, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED);


  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @Mock
  private ProductMailEventsRepository productMailEventsRepository;

  @Mock
  private MarginRepository marginRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductMailEventPublisher productMailEventPublisher;

  @Mock
  private ProductLevel3WipRepository productLevel3WipRepository;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductNotificationService productNotificationService;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @InjectMocks
  private ProductMailEventServiceImpl productMailEventService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private MarginFeign marginFeign;

  @Captor
  private ArgumentCaptor<FilterMarginsByOrderItemsRequest> marginOrderItemArgumentCaptor;


  private List<ProductLevel3Wip> productLevel3Wips;

  private ProductCollection productCollection;

  private List<ProductMailEvents> postLiveReviewActiveMailEventList;

  private ProfileResponse profileResponse;
  private Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
  private Map<String, List<List<String>>> emails;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productLevel3Wips = new ArrayList<>();
    ProductLevel3Wip productLevel3Wip = new ProductLevel3Wip();
    productLevel3Wip.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3Wip.setProductLevel1Id(PRODUCT_ID);
    productLevel3Wip.setState(STATE);
    productLevel3Wip.setProductSku(PRODUCT_SKU);
    productLevel3Wip.setCreatedDate(new Date());
    ProductLevel3ItemWip productLevel3ItemWip = new ProductLevel3ItemWip();
    productLevel3Wip.setItems(Arrays.asList(productLevel3ItemWip));
    productLevel3Wips.add(productLevel3Wip);
    emails = new HashMap<>();

    MDC.put("storeId" , STORE_ID);

    productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productCollection.setCreatedDate(new Date());
    productCollection.setCategoryCode(NEW_CATEGORY_CODE);

    ProductMailEvents rejectMailEvents = ProductMailEvents.builder().events(ProductMailEventsEnum.REJECTED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).build();
    ProductMailEvents activeMailEvents = ProductMailEvents.builder().events(ProductMailEventsEnum.APPROVED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).build();
    activeMailEvents.setCreatedDate(new Date());
    ProductMailEvents categoryChangeMailEvents = ProductMailEvents.builder()
        .events(ProductMailEventsEnum.CATEGORY_CHANGE).productSku(PRODUCT_SKU)
        .productCode(PRODUCT_CODE).notes(NOTES).build();
    ProductMailEvents postLiveReviewActiveMailEvent = ProductMailEvents.builder()
        .events(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).build();
    postLiveReviewActiveMailEvent.setCreatedDate(new Date());

    ProductMailEvents postLiveReviewActiveMailEvent2 = ProductMailEvents.builder()
        .events(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED)
        .productSku(PRODUCT_SKU_1).productCode(PRODUCT_CODE_1).build();
    postLiveReviewActiveMailEvent2.setCreatedDate(new Date());

    ProductMailEvents postLiveReviewRejectMailEvent = ProductMailEvents.builder()
        .events(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).notes(NOTES).build();
    postLiveReviewRejectMailEvent.setCreatedDate(new Date());

    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setGeneratedItemName(PRODUCT_ITEM_NAME);
    productDetailResponse.setProductItemResponses(new HashSet<>(Arrays.asList(productItemResponse)));

    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(Boolean.FALSE);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);

    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    ItemResponse item = new ItemResponse();
    item.setItemSku(ITEM_SKU);
    productAndItemsResponse.setItems(Arrays.asList(item));

    Map<String, String> itemNameBySkus = new HashMap<>();
    itemNameBySkus.put(ITEM_SKU,PRODUCT_ITEM_NAME);

    ProductMailEvents suspendedProduct = ProductMailEvents.builder().events(ProductMailEventsEnum.SUSPENDED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).notes(NOTES).build();
    ProductMailEvents activatedProduct = ProductMailEvents.builder().events(ProductMailEventsEnum.RE_ACTIVATED)
        .productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).notes(NOTES).build();

    postLiveReviewActiveMailEventList = new ArrayList<>();
    postLiveReviewActiveMailEventList.add(postLiveReviewActiveMailEvent2);
    postLiveReviewActiveMailEventList.add(postLiveReviewActiveMailEvent);

    when(this.productMailEventsRepository.save(any(ProductMailEvents.class)))
        .thenReturn(null);
    when(this.productLevel3WipService.findProductL3WipByStoreIdAndProductCode(anyString(),
        eq(PRODUCT_CODE))).thenReturn(productLevel3Wips);
    when(this.productMailEventPublisher.publishProductMailDomainEventCategoryChange(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventActive(any()))
      .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventRejected(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventSendForCorrection(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventCategoryChangeEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventActiveEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventRejectedEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishProductMailDomainEventSendForCorrectionEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemSuspensionMailEventEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemSuspensionMailEvent(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemReActivationMailEvent(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemReActivationMailEventEn(any()))
        .thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemSkuArchivedEvent(any())).thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventPublisher.publishItemSkuArchivedEventEn(any())).thenReturn(new ProductMailDomainEvent());
    when(this.productMailEventsRepository.findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(
        anyString() , any())).thenReturn(Arrays.asList(BUSINESS_PARTNER_CODE));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString() , eq(ProductMailEventsEnum.REJECTED) , any()))
        .thenReturn(Arrays.asList(rejectMailEvents));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString() , eq(ProductMailEventsEnum.APPROVED) , any()))
        .thenReturn(Arrays.asList(activeMailEvents));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(anyString(), anyString(),
            eq(APPROVED_EVENTS), any()))
        .thenReturn(Arrays.asList(activeMailEvents));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString() , eq(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED) , any()))
        .thenReturn(Collections.singletonList(postLiveReviewActiveMailEvent));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString() , eq(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED) , any()))
        .thenReturn(Collections.singletonList(postLiveReviewRejectMailEvent));
    when(this.productMailEventsRepository
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(anyString(),
            anyString() , eq(ProductMailEventsEnum.CATEGORY_CHANGE)))
        .thenReturn(categoryChangeMailEvents);
    when(this.productMailEventsRepository
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(anyString(),
            anyString() , eq(ProductMailEventsEnum.CATEGORY_CHANGE)))
        .thenReturn(categoryChangeMailEvents);
    when(productLevel3WipRepository.findByStoreIdAndProductSku(anyString(), anyString()))
        .thenReturn(productLevel3Wip);
    when(productLevel3WipService.getProductLevel3WipByProductSkuWithItemsInitialised(anyString(), anyString()))
        .thenReturn(productLevel3Wip);
    when(this.productRepository.findProductDetailByProductCode(anyString()))
        .thenReturn(productDetailResponse);
    when(this.productRepository.findProductDetailByProductCode(anyString(), eq(true)))
        .thenReturn(productDetailResponse);
    when(this.objectMapper.readValue(eq(NOTES), eq(CategoryChangeMailEvent.class)))
        .thenReturn(new CategoryChangeMailEvent());
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString(), eq(ProductMailEventsEnum.SUSPENDED), any()))
        .thenReturn(Arrays.asList(suspendedProduct));
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(),
            anyString(), eq(ProductMailEventsEnum.RE_ACTIVATED), any()))
        .thenReturn(Arrays.asList(activatedProduct));
    when(this.productLevel3Repository
        .findDetailByProductSkuForSuspension(eq(PRODUCT_SKU))).thenReturn(productAndItemsResponse);
    when(this.productLevel3Repository.getItemNameByItemSku(eq(ITEM_SKU_LIST))).thenReturn(itemNameBySkus);
    when(this.productMailEventsRepository
        .findDistinctBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(anyString(), any(), eq(EVENTS)))
        .thenReturn(Arrays.asList(BUSINESS_PARTNER_CODE));

    profileResponseMap.put(BUSINESS_PARTNER_CODE, profileResponse);
    ITEM_SKU_LIST.add(ITEM_SKU);
    ITEM_NAME_LIST.add(PRODUCT_ITEM_NAME);
    PRODUCT_ITEM_DATA.add(ITEM_SKU_LIST);
    PRODUCT_ITEM_DATA.add(ITEM_NAME_LIST);
    emails.put(BUSINESS_PARTNER_CODE, PRODUCT_ITEM_DATA);

    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productLevel3WipService);
    verifyNoMoreInteractions(this.productMailEventsRepository);
    verifyNoMoreInteractions(this.businessPartnerRepository);
    verifyNoMoreInteractions(this.marginRepository);
    verifyNoMoreInteractions(this.objectMapper);
    verifyNoMoreInteractions(this.productMailEventPublisher);
    verifyNoMoreInteractions(this.productLevel3WipRepository);
    verifyNoMoreInteractions(this.productRepository);
    verifyNoMoreInteractions(this.productNotificationService);
    verifyNoMoreInteractions(this.productLevel3Repository);
    verifyNoMoreInteractions(this.mandatoryParameterHelper);
  }

  @Test
  public void createAndSaveMailRejectEvent() throws Exception {
    this.productMailEventService.createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.REJECTED);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(any() , eq(PRODUCT_CODE));
    verify(this.productMailEventsRepository)
        .save(productMailEventsArgumentCaptor.capture());
    verify(this.productNotificationService)
        .sendProductRejectNotification(eq(BUSINESS_PARTNER_CODE), any());
    ProductMailEvents productMailEvents = productMailEventsArgumentCaptor.getValue();
    assertEquals(productMailEvents.getProductCode(), PRODUCT_CODE);
  }

  @Test
  public void createAndSaveMailEvent() throws Exception {
    this.productMailEventService.createAndSaveMailEvent(PRODUCT_CODE, NOTES,
        ProductMailEventsEnum.APPROVED);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(anyString(), eq(PRODUCT_CODE));
    verify(this.productMailEventsRepository)
        .save(productMailEventsArgumentCaptor.capture());
    ProductMailEvents productMailEvents = productMailEventsArgumentCaptor.getValue();
    assertEquals(productMailEvents.getProductCode(), PRODUCT_CODE);
    assertEquals(productMailEvents.getProductSku() , PRODUCT_SKU);
    assertEquals(productMailEvents.getNotes(), NOTES);
  }

  @Test
  public void createAndSaveMailEvent_Exception() throws Exception {
    when(this.productLevel3WipService.findProductL3WipByStoreIdAndProductCode(anyString(),
        eq(PRODUCT_CODE))).thenThrow(Exception.class);
    try {
      this.productMailEventService.createAndSaveMailEvent(PRODUCT_CODE, NOTES,
          ProductMailEventsEnum.REJECTED);
    } catch (Exception ex) { }
    finally {
      verify(this.productLevel3WipService).findProductL3WipByStoreIdAndProductCode(eq(STORE_ID) ,
          eq(PRODUCT_CODE));
    }
  }

  @Test
  public void sendReturnForCorrectionDomainEventTest() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    this.productMailEventService.sendDomainEventForSentForCorrection(PRODUCT_CODE , NOTES);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventPublisher).publishProductMailDomainEventSendForCorrection(
        this.productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(eq(STORE_ID) , eq(PRODUCT_CODE));
    ProductMailDomainEvent domainEvent = this.productMailDomainEventArgumentCaptor.getValue();
    assertEquals(domainEvent.getMerchantCode() ,BUSINESS_PARTNER_CODE);
    assertEquals(domainEvent.getProductSku() , PRODUCT_CODE);
    assertEquals(domainEvent.getNotificationType() , ProductMailEventsEnum.SENT_FOR_CORRECTION.getNotificationType() + "-in");
  }

  @Test
  public void sendReturnForCorrectionDomainEventTest_forInternational() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    this.productMailEventService.sendDomainEventForSentForCorrection(PRODUCT_CODE , NOTES);
    verify(this.productMailEventPublisher).publishProductMailDomainEventSendForCorrectionEn(
        this.productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = this.productMailDomainEventArgumentCaptor.getValue();
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(eq(STORE_ID) , eq(PRODUCT_CODE));
    assertEquals(domainEvent.getMerchantCode() ,BUSINESS_PARTNER_CODE);
    assertEquals(domainEvent.getProductSku() , PRODUCT_CODE);
    assertEquals(domainEvent.getNotificationType() , ProductMailEventsEnum.SENT_FOR_CORRECTION.getNotificationType() + "-en");
  }

  @Test
  public void createAndSaveCategoryChangeMailEventTest() throws Exception {
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    when(this.marginRepository.getMarginForStoreIdAndCategoryCode(anyString() , anyString()))
        .thenAnswer(invocationOnMock -> {
          String categoryCode = invocationOnMock.getArguments()[1].toString();
          MarginCategoryResponse response = new MarginCategoryResponse();
          response.setCategoryId(categoryCode);
          response.setValue(1.0);
          return response;
        });

    productMailEventService
        .createAndSaveCategoryChangeMailEvent(productCollection ,categoryChangeMailEvent);

    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(anyString() , eq(PRODUCT_CODE));
    ArgumentCaptor<String> categoryCodeCaptor = ArgumentCaptor.forClass(String.class);
    verify(this.marginRepository ,times(2))
        .getMarginForStoreIdAndCategoryCode(eq(STORE_ID) ,categoryCodeCaptor.capture());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
    ProductMailEvents productMailEvents = productMailEventsArgumentCaptor.getValue();
    assertEquals(productMailEvents.getProductCode(), PRODUCT_CODE);
    assertEquals(productMailEvents.getProductSku() , PRODUCT_SKU);
    assertEquals(productMailEvents.getEvents() , ProductMailEventsEnum.CATEGORY_CHANGE);
    List<String> categoryCodes = categoryCodeCaptor.getAllValues();
    assertTrue(categoryCodes.contains(OLD_CATEGORY_CODE));
    assertTrue(categoryCodes.contains(NEW_CATEGORY_CODE));
  }

  @Test
  public void createAndSaveCategoryChangeMailEventOldCategoryNullTest() throws Exception {
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    when(this.marginRepository.getMarginForStoreIdAndCategoryCode(anyString() , eq(NEW_CATEGORY_CODE)))
        .thenAnswer(invocationOnMock -> {
          String categoryCode = invocationOnMock.getArguments()[1].toString();
          MarginCategoryResponse response = new MarginCategoryResponse();
          response.setCategoryId(categoryCode);
          response.setValue(1.0);
          return response;
        });

    productMailEventService
        .createAndSaveCategoryChangeMailEvent(productCollection ,categoryChangeMailEvent);

    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(anyString() , eq(PRODUCT_CODE));
    ArgumentCaptor<String> categoryCodeCaptor = ArgumentCaptor.forClass(String.class);
    verify(this.marginRepository ,times(2))
        .getMarginForStoreIdAndCategoryCode(eq(STORE_ID) ,categoryCodeCaptor.capture());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
    ProductMailEvents productMailEvents = productMailEventsArgumentCaptor.getValue();
    assertEquals(productMailEvents.getProductCode(), PRODUCT_CODE);
    assertEquals(productMailEvents.getProductSku() , PRODUCT_SKU);
    assertEquals(productMailEvents.getEvents() , ProductMailEventsEnum.CATEGORY_CHANGE);
    List<String> categoryCodes = categoryCodeCaptor.getAllValues();
    assertTrue(categoryCodes.contains(OLD_CATEGORY_CODE));
    assertTrue(categoryCodes.contains(NEW_CATEGORY_CODE));
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewCategoryNullTest() throws Exception {
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    when(this.marginRepository.getMarginForStoreIdAndCategoryCode(anyString() , eq(OLD_CATEGORY_CODE)))
        .thenAnswer(invocationOnMock -> {
          String categoryCode = invocationOnMock.getArguments()[1].toString();
          MarginCategoryResponse response = new MarginCategoryResponse();
          response.setCategoryId(categoryCode);
          response.setValue(1.0);
          return response;
        });

    productMailEventService
        .createAndSaveCategoryChangeMailEvent(productCollection ,categoryChangeMailEvent);

    ArgumentCaptor<String> categoryCodeCaptor = ArgumentCaptor.forClass(String.class);
    verify(this.marginRepository ,times(2))
        .getMarginForStoreIdAndCategoryCode(eq(STORE_ID) ,categoryCodeCaptor.capture());
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    ReflectionTestUtils.setField(productMailEventService, "setDefaultOrderTypeForMargin", true);
    ReflectionTestUtils.setField(productMailEventService, "defaultOrderTypeForMargin", Constants.B2C_RETAIL);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        marginOrderItemArgumentCaptor.capture())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
    Assertions.assertEquals(marginOrderItemArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType(),
        Constants.B2C_RETAIL);
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewFlafFalseChanges() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    ReflectionTestUtils.setField(productMailEventService, "setDefaultOrderTypeForMargin", false);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        marginOrderItemArgumentCaptor.capture())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
    Assertions.assertNull(marginOrderItemArgumentCaptor.getValue().getMarginOrderItem().get(0).getOrderType());
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges1() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);
    productCollection.setCategoryCode(OLD_CATEGORY_CODE);
    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
  }

  @Test
  public void getMarginBusinessPartnerWebResponseNullTest(){
    productMailEventService.getMarginBusinessPartnerWebResponse(null,new CategoryChangeMailEvent());
  }

  @Test
  public void getMarginBusinessPartnerWebResponseForExistingNullTest() {
    productMailEventService.getMarginBusinessPartnerWebResponseForExisting(null, new CategoryChangeMailEvent());
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges4() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("BASE-ADDON");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges5() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("SPECIAL-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges6() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("BASE-MARGIN");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
    verify(this.productLevel3WipService)
        .findProductL3WipByStoreIdAndProductCode(Mockito.any(),Mockito.any());
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    verify(this.objectMapper).writeValueAsString(eq(categoryChangeMailEvent));
  }

  @Test
  public void createAndSaveCategoryChangeMailEventNewChanges7() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    Margin margin = new Margin();
    margin.setMarginType("-");
    margin.setReplacementType("-");
    marginList.add(margin);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);

    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
  }

  @Test
  public void createAndSaveCategoryChangeMailEventTest_Exception() throws Exception {
    when(this.marginRepository.getMarginForStoreIdAndCategoryCode(anyString() , anyString()))
        .thenThrow(Exception.class);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);
    try{
      this.productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection ,
          categoryChangeMailEvent);
    }catch (Exception ex){}
    finally {
      verify(this.marginRepository)
          .getMarginForStoreIdAndCategoryCode(STORE_ID , OLD_CATEGORY_CODE);
    }
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersTest() throws Exception {
    this.productMailEventService.sendProductMailEventsToBusinessPartners(new Date());
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(eq(STORE_ID), any());
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE), eq(ProductMailEventsEnum.REJECTED), any());
    verify(this.productMailEventPublisher).publishProductMailDomainEventRejected(
        productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.REJECTED.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_CODE, productDatas.get(0).get(1));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE),
            eq(APPROVED_EVENTS), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productLevel3WipService)
        .getProductLevel3WipByProductSkuWithItemsInitialised(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productRepository).findProductDetailByProductCode(eq(PRODUCT_CODE));
    verify(this.productMailEventPublisher).publishProductMailDomainEventActive(
        productMailDomainEventArgumentCaptor.capture());
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.APPROVED.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    verify(this.productMailEventPublisher).publishProductMailDomainEventCategoryChange(
        productMailDomainEventArgumentCaptor.capture());
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.CATEGORY_CHANGE.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    verify(this.objectMapper, times(2)).readValue(eq(NOTES), eq(CategoryChangeMailEvent.class));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersTest_ProductNotFound() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    this.productMailEventService.sendProductMailEventsToBusinessPartners(new Date());
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(eq(STORE_ID), any());
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE), eq(ProductMailEventsEnum.REJECTED), any());
    verify(this.productMailEventPublisher).publishProductMailDomainEventRejected(
        productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.REJECTED.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_CODE, productDatas.get(0).get(1));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE),
            eq(APPROVED_EVENTS), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productLevel3WipService)
        .getProductLevel3WipByProductSkuWithItemsInitialised(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productRepository).findProductDetailByProductCode(eq(PRODUCT_CODE));
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.REJECTED.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertNull(productDatas.get(0).get(2));
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.REJECTED.getNotificationType() + "-in",
        domainEvent.getNotificationType());
    assertNull(productDatas.get(0).get(2));
    verify(this.objectMapper, times(2)).readValue(eq(NOTES), eq(CategoryChangeMailEvent.class));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersTest_NoInitialMail() throws Exception {
    when(this.productMailEventsRepository
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(anyString(), anyString(),
            eq(ProductMailEventsEnum.CATEGORY_CHANGE))).thenReturn(null);
    doThrow(ApplicationRuntimeException.class).when(productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    this.productMailEventService.sendProductMailEventsToBusinessPartners(new Date());
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(eq(STORE_ID), any());
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.REJECTED), any());
    verify(this.productMailEventPublisher)
        .publishProductMailDomainEventRejected(productMailDomainEventArgumentCaptor.capture());
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE), eq(APPROVED_EVENTS), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productLevel3WipService)
        .getProductLevel3WipByProductSkuWithItemsInitialised(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(eq(STORE_ID), eq(PRODUCT_SKU),
            eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productRepository).findProductDetailByProductCode(eq(PRODUCT_CODE));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendArchivedItemSkuDueToOosMailTest() throws Exception {
    this.productMailEventService.sendMailForArchivedItemSkuDueToOos(emails, profileResponseMap);
    verify(this.productMailEventPublisher).publishItemSkuArchivedEvent(
        productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.AUTO_ARCHIVED.getNotificationType() + "-in", domainEvent.getNotificationType());
    assertEquals(ITEM_SKU, productDatas.get(0).get(0));
  }

  @Test
  public void sendArchivedItemSkuDueToOosMailEnTest() throws Exception {
    emails.put(BUSINESS_PARTNER_CODE, PRODUCT_ITEM_DATA);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse.setCompany(companyDTO);
    this.productMailEventService.sendMailForArchivedItemSkuDueToOos(emails, profileResponseMap);
    verify(this.productMailEventPublisher).publishItemSkuArchivedEventEn(
        productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.AUTO_ARCHIVED.getNotificationType() + "-en", domainEvent.getNotificationType());
    assertEquals(ITEM_SKU, productDatas.get(0).get(0));
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersTest_En() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    this.productMailEventService.sendProductMailEventsToBusinessPartners(new Date());
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(eq(STORE_ID), any());
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE), eq(ProductMailEventsEnum.REJECTED), any());
    verify(this.productMailEventPublisher).publishProductMailDomainEventRejectedEn(
        productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.REJECTED.getNotificationType() + "-en",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_CODE, productDatas.get(0).get(1));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsInAndCreatedDateGreaterThan(eq(STORE_ID),
            eq(BUSINESS_PARTNER_CODE),
            eq(APPROVED_EVENTS), any());
    verify(this.productLevel3WipRepository).findByStoreIdAndProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productLevel3WipService)
        .getProductLevel3WipByProductSkuWithItemsInitialised(eq(STORE_ID), eq(PRODUCT_SKU));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateDesc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productMailEventsRepository)
        .findTopByStoreIdAndProductSkuAndEventsOrderByCreatedDateAsc(eq(STORE_ID),
            eq(PRODUCT_SKU), eq(ProductMailEventsEnum.CATEGORY_CHANGE));
    verify(this.productRepository).findProductDetailByProductCode(eq(PRODUCT_CODE));
    verify(this.productMailEventPublisher).publishProductMailDomainEventActiveEn(
        productMailDomainEventArgumentCaptor.capture());
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.APPROVED.getNotificationType() + "-en",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    verify(this.productMailEventPublisher).publishProductMailDomainEventCategoryChangeEn(
        productMailDomainEventArgumentCaptor.capture());
    domainEvent = productMailDomainEventArgumentCaptor.getValue();
    productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.CATEGORY_CHANGE.getNotificationType() + "-en",
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    verify(this.objectMapper, times(2)).readValue(eq(NOTES), eq(CategoryChangeMailEvent.class));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void createAndSaveMailSuspensionOrReactivationEventTest() throws Exception {
    this.productMailEventService
        .createAndSaveMailEventForSuspensionOrReActivation(ProductMailEventsEnum.RE_ACTIVATED, BUSINESS_PARTNER_CODE,
            NOTES, PRODUCT_CODE, PRODUCT_SKU);
    verify(this.productMailEventsRepository).save(productMailEventsArgumentCaptor.capture());
    ProductMailEvents productMailEvents = productMailEventsArgumentCaptor.getValue();
    assertEquals(productMailEvents.getProductCode(), PRODUCT_CODE);
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersForSuspensionTest() throws Exception {
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    this.productMailEventService.sendProductMailEventsToBusinessPartnersForSuspension(new Date());
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), any(), eq(EVENTS));
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.SUSPENDED), any());
    verify(this.productLevel3Repository, times(2)).findDetailByProductSkuForSuspension(eq(PRODUCT_SKU));
    verify(this.productLevel3Repository, times(2)).getItemNameByItemSku(eq(ITEM_SKU_LIST));
    verify(this.productMailEventPublisher)
        .publishItemSuspensionMailEvent(productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent suspensionDomainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> suspensionProductDatas = suspensionDomainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.SUSPENDED.getNotificationType() + "-in",
        suspensionDomainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, suspensionProductDatas.get(0).get(1));
    assertEquals(NOTES, suspensionProductDatas.get(0).get(2));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.RE_ACTIVATED), any());
    verify(this.productMailEventPublisher)
        .publishItemReActivationMailEvent(productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent reActivationDomainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> reActivationProductDatas = reActivationDomainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.RE_ACTIVATED.getNotificationType() + "-in",
        reActivationDomainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, reActivationProductDatas.get(0).get(1));
    assertEquals(NOTES, reActivationProductDatas.get(0).get(2));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendProductMailEventsToBusinessPartnersForSuspensionTestEn() throws Exception {
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    this.productMailEventService.sendProductMailEventsToBusinessPartnersForSuspension(new Date());
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), any(), eq(EVENTS));
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(eq(BUSINESS_PARTNER_CODE));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.SUSPENDED), any());
    verify(this.productLevel3Repository, times(2)).findDetailByProductSkuForSuspension(eq(PRODUCT_SKU));
    verify(this.productLevel3Repository, times(2)).getItemNameByItemSku(eq(ITEM_SKU_LIST));
    verify(this.productMailEventPublisher)
        .publishItemSuspensionMailEventEn(productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent suspensionDomainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> suspensionProductDatas = suspensionDomainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.SUSPENDED.getNotificationType() + "-en",
        suspensionDomainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, suspensionProductDatas.get(0).get(1));
    assertEquals(NOTES, suspensionProductDatas.get(0).get(2));
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.RE_ACTIVATED), any());
    verify(this.productMailEventPublisher)
        .publishItemReActivationMailEventEn(productMailDomainEventArgumentCaptor.capture());
    ProductMailDomainEvent reActivationDomainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> reActivationProductDatas = reActivationDomainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.RE_ACTIVATED.getNotificationType() + "-en",
        reActivationDomainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, reActivationProductDatas.get(0).get(1));
    assertEquals(NOTES, reActivationProductDatas.get(0).get(2));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendPostLiveReviewActiveProductMailEventsToBusinessPartnersTest() throws Exception {
    Date date = new Date();
    this.productMailEventService.sendPostLiveReviewActiveProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(STORE_ID,
            BUSINESS_PARTNER_CODE, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED, date);
    verify(this.productMailEventPublisher).publishPostLiveReviewApprovedProductMailDomainEvent(
        productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    verify(this.mandatoryParameterHelper).getStoreId();
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType() + LANGUAGE_IN,
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    assertEquals(PRODUCT_SKU, productDatas.get(0).get(3));
  }

  @Test
  public void sendPostLiveReviewActiveProductMailEventsToBusinessPartnersExceptionTest() throws Exception {
    when(this.productMailEventsRepository
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(eq(STORE_ID), eq(BUSINESS_PARTNER_CODE),
            eq(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED), any())).thenReturn(postLiveReviewActiveMailEventList);
    doThrow(ApplicationRuntimeException.class).when(productRepository).findProductDetailByProductCode(PRODUCT_CODE_1);
    Date date = new Date();
    this.productMailEventService.sendPostLiveReviewActiveProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(STORE_ID,
            BUSINESS_PARTNER_CODE, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED, date);
    verify(this.productMailEventPublisher).publishPostLiveReviewApprovedProductMailDomainEvent(
        productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU);
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU_1);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE_1);
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType() + LANGUAGE_IN,
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    assertEquals(PRODUCT_SKU, productDatas.get(0).get(3));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendPostLiveReviewActiveProductMailEventsToBusinessPartnersEnTest() throws Exception {
    Date date = new Date();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse.setCompany(companyDTO);
    this.productMailEventService.sendPostLiveReviewActiveProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(STORE_ID,
            BUSINESS_PARTNER_CODE, ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED, date);
    verify(this.productMailEventPublisher).publishPostLiveReviewApprovedProductMailDomainEventEn(
        productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE);
    verify(this.mandatoryParameterHelper).getStoreId();
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED.getNotificationType() + LANGUAGE_EN,
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    assertEquals(PRODUCT_SKU, productDatas.get(0).get(3));
  }

  @Test
  public void sendPostLiveReviewRejectProductMailEventsToBusinessPartnersTest() throws Exception {
    Date date = new Date();
    this.productMailEventService.sendPostLiveReviewRejectProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(STORE_ID,
            BUSINESS_PARTNER_CODE, ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED, date);
    verify(this.productMailEventPublisher).publishPostLiveReviewRejectedProductMailDomainEvent(
        productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE, true);
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType() + LANGUAGE_IN,
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    assertEquals(NOTES, productDatas.get(0).get(3));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendPostLiveReviewRejectProductMailEventsToBusinessPartnersExceptionTest() throws Exception {
    Date date = new Date();
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenThrow(Exception.class);
    this.productMailEventService.sendPostLiveReviewRejectProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository).findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void sendPostLiveReviewRejectProductMailEventsToBusinessPartnersEnTest() throws Exception {
    Date date = new Date();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(true);
    profileResponse.setCompany(companyDTO);
    this.productMailEventService.sendPostLiveReviewRejectProductMailEventsToBusinessPartners(date);
    verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(this.productMailEventsRepository)
        .findDistinctBusinessPartnerCodeByCreatedDateGreaterThan(STORE_ID, date);
    verify(this.productMailEventsRepository)
        .findByStoreIdAndBusinessPartnerCodeAndEventsAndCreatedDateGreaterThan(STORE_ID,
            BUSINESS_PARTNER_CODE, ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED, date);
    verify(this.productMailEventPublisher).publishPostLiveReviewRejectedProductMailDomainEventEn(
        productMailDomainEventArgumentCaptor.capture());
    verify(this.productLevel3WipService).getProductLevel3WipByProductSkuWithItemsInitialised(STORE_ID, PRODUCT_SKU);
    verify(this.productRepository).findProductDetailByProductCode(PRODUCT_CODE, true);
    ProductMailDomainEvent domainEvent = productMailDomainEventArgumentCaptor.getValue();
    List<List<String>> productDatas = domainEvent.getProductDatas();
    assertEquals(ProductMailEventsEnum.POST_LIVE_REVIEW_REJECTED.getNotificationType() + LANGUAGE_EN,
        domainEvent.getNotificationType());
    assertEquals(PRODUCT_ITEM_NAME, productDatas.get(0).get(2));
    assertEquals(NOTES, productDatas.get(0).get(3));
    verify(this.mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void createAndSaveCategoryChangeMailEventMarginNullNewChanges() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "marginNewFilterApiEnabled", true);
    CategoryChangeMailEvent categoryChangeMailEvent = new CategoryChangeMailEvent();
    categoryChangeMailEvent.setExistingCategoryCode(OLD_CATEGORY_CODE);
    categoryChangeMailEvent.setNewCategoryCode(NEW_CATEGORY_CODE);

    ListBaseResponse<OrderItemMarginsResponse> listBaseResponse = new ListBaseResponse<>();
    List<OrderItemMarginsResponse> orderItemMarginsResponses = new ArrayList<>();
    OrderItemMarginsResponse orderItemMarginsResponse = new OrderItemMarginsResponse();
    List<Margin> marginList = new ArrayList<>();
    marginList.add(null);
    orderItemMarginsResponse.setMargins(marginList);
    orderItemMarginsResponses.add(orderItemMarginsResponse);
    listBaseResponse.setContent(orderItemMarginsResponses);
    listBaseResponse.setSuccess(true);
    Mockito.when(marginFeign.filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(listBaseResponse);
    productMailEventService.createAndSaveCategoryChangeMailEvent(productCollection, categoryChangeMailEvent);
    Mockito.verify(marginFeign, times(2))
        .filterMargin(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void testDeleteOldRecordsByDaysTest() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "deleteMailEventBatchSize", 50);
    Mockito.when(productMailEventsRepository.deleteOldRecords(1, 50)).thenReturn(5).thenReturn(5).thenReturn(0);
    productMailEventService.deleteOldRecordsByDays(1);
    Mockito.verify(productMailEventsRepository, times(3)).deleteOldRecords(1, 50);
  }

  @Test
  public void testDeleteOldRecordsByDaysTestWithNoData() throws Exception {
    ReflectionTestUtils.setField(productMailEventService, "deleteMailEventBatchSize", 50);
    Mockito.when(productMailEventsRepository.deleteOldRecords(1, 50)).thenReturn(0);
    productMailEventService.deleteOldRecordsByDays(1);
    Mockito.verify(productMailEventsRepository, times(1)).deleteOldRecords(1, 50);
  }

}