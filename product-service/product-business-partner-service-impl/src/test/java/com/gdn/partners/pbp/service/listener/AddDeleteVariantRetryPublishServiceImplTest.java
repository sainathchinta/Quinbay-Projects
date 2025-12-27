package com.gdn.partners.pbp.service.listener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Date;

import com.gdn.mta.product.service.config.PreOrderConfig;
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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductRepositoryBean;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ScheduledJobServiceImpl;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.domainevent.AddDeleteVariantRetryPublishServiceImpl;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.mta.product.util.CommonUtils;
import org.mockito.MockedStatic;


public class AddDeleteVariantRetryPublishServiceImplTest {

  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final String ATTRIBUTE_VALUE_1 = "ATTRIBUTE_VALUE_1";
  private static final String ATTRIBUTE_ID_2 = "ATTRIBUTE_ID_2";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private static final String PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";
  private static final String ITEM_ID = "ITEM_ID";
  private static final String BUSINESS_PARTER_CODE = "BUSINESS_PARTER_CODE";
  private static final String GDN_PRODUCT_SKU = "GDN_PRODUCT_SKU";
  private static final String ITEM_CODE = "ITEM_CODE";

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductRepositoryBean productRepositoryBean;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private PickupPointOutbound pickupPointOutbound;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Mock
  private ScheduledJobServiceImpl scheduledJobService;

  @InjectMocks
  private AddDeleteVariantRetryPublishServiceImpl addDeleteVariantRetryPublishServiceImpl;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private PreOrderConfig preOrderConfig;

  @Captor
  private ArgumentCaptor<com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest>
      addDeleteVariantRetryRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductLevel3Inventory>> productLevel3InventoryCaptor;

  private ProductCollection productCollection = new ProductCollection();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties, productService, productBusinessPartnerRepository,
        productRepositoryBean, xProductOutbound, businessPartnerRepository, pickupPointOutbound,
        productLevel3InventoryService, productLevel3LogisticsService, scheduledJobService);
  }

  private ProductBusinessPartner getProductBusinessPartner() {
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setProductType(1);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner1.setInstallation(true);
    productItemBusinessPartner1.setProductItemId(PRODUCT_ITEM_ID);
    productItemBusinessPartner1.setSalePrice(1000.0);
    productItemBusinessPartner1.setPrice(1000.0);
    productItemBusinessPartner1.setBuyable(true);
    productItemBusinessPartner1.setDisplay(true);
    productItemBusinessPartner1.setMarkForDelete(false);
    productItemBusinessPartner1.setProductItemId(ITEM_ID);
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute1 = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute1.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttribute1.setValue(ATTRIBUTE_VALUE_1);
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute2 = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute2.setAttributeId(ATTRIBUTE_ID_2);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(GDN_PRODUCT_SKU);
    productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTER_CODE);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner1));
    productBusinessPartner.setProductBusinessPartnerAttributes(
        Arrays.asList(productBusinessPartnerAttribute1, productBusinessPartnerAttribute2));
    return productBusinessPartner;
  }

  @Test
  public void addDeleteVariantRetryPublishListenerTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode("BLIBLI-TD");
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService).updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService).insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE, logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Assertions.assertEquals(ITEM_CODE, productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }


  @Test
  public void addDeleteVariantRetryPublishListenerProductBusinessPartnerisEmptyTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(null);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void addDeleteVariantRetryPublishListenerExceptionTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenThrow(RuntimeException.class);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.FAILED);
  }

  @Test
  public void addDeleteVariantRetryPublishListenerPurchaseTermNotPOTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("POM");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTER_CODE);
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE)).thenReturn(null);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "mppForWhEnabled", true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService).insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE, logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Assertions.assertEquals(ITEM_CODE, productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }

  @Test
  public void addDeleteVariantRetryPublishListenerMppEnableFalseTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("POM");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTER_CODE);
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE)).thenReturn(null);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "mppForWhEnabled", false);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService).insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE, logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Assertions.assertEquals(ITEM_CODE, productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }

  @Test
  public void addDeleteVariantRetryPublishListenerNUllPickupPointResponseTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode("pp");
    pickupPointResponse.setFbbActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("POM");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTER_CODE);
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE)).thenReturn(null);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "mppForWhEnabled", false);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE, logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Assertions.assertEquals(PRODUCT_CODE, addDeleteVariantRetryRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void addDeleteVariantRetryPublishListenerProductExistsInPDTFalseTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode("pp");
    pickupPointResponse.setFbbActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("POM");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTER_CODE);
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(false);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE)).thenReturn(null);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "mppForWhEnabled", false);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound).getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3LogisticsService).findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE, logistics, true);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Assertions.assertEquals(PRODUCT_CODE, addDeleteVariantRetryRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void addDeleteVariantRetryPublishListenerProductExistsPickupPointEmptyTest() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode("pp");
    pickupPointResponse.setFbbActivated(true);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("POM");
    profileResponse.setCompany(companyDTO);
    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode(BUSINESS_PARTER_CODE);
    inventoryList.add(productLevel3Inventory);
    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
        Mockito.eq(GDN_PRODUCT_SKU))).thenReturn(new ArrayList<>());
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
        Collections.singletonList(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true)).thenReturn(true);
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(false);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE)).thenReturn(null);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "mppForWhEnabled", false);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl, "pickupPointFetchBatchSizeForReconcilation",
        1);
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean).getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.SUCCESS);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Assertions.assertEquals(PRODUCT_CODE, addDeleteVariantRetryRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void processAddDeleteVariantRetryEventsTest(){
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(new AddDeleteVariantRetryPublishEventModel());
  }

  @Test
  public void addDeleteVariantRetryPublishListenerProductCollectionTest1() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(null);
    Mockito.when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE,AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void addDeleteVariantRetryPublishListener_WithPreOrderDates_Test() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    profileResponse.setCompany(companyDTO);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse1 = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse1.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse1.setPickupPointCode(PICKUP_POINT_CODE);
    Date preOrderDate1 = new Date(System.currentTimeMillis() + 86400000);
    itemPickupPointCodeResponse1.setPreOrderDate(preOrderDate1);

    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse1);

    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode("BLIBLI-TD");
    productLevel3Inventory.setPreOrderDate(preOrderDate1);
    inventoryList.add(productLevel3Inventory);

    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,
        AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(
            productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(
            productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(
            addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU)))
        .thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
                Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(
            productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU),
            BUSINESS_PARTER_CODE, logistics, true)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "pickupPointFetchBatchSizeForReconcilation", 1);

    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(
        addDeleteVariantRetryPublishEventModel);

    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository)
        .findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean)
        .getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
            Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
            Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService)
        .insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.SUCCESS);

    Assertions.assertEquals(ITEM_CODE,
        productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }

  @Test
  public void addDeleteVariantRetryPublishListener_WithNullPreOrderDates_Test() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    profileResponse.setCompany(companyDTO);

    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse1 = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse1.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeResponse1.setPreOrderDate(null); // NULL pre-order date

    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse1);

    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode("BLIBLI-TD");
    inventoryList.add(productLevel3Inventory);

    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,
        AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(
            productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(
            productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(
            addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU)))
        .thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
                Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(
            productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU),
            BUSINESS_PARTER_CODE, logistics, true)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "pickupPointFetchBatchSizeForReconcilation", 1);

    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(
        addDeleteVariantRetryPublishEventModel);

    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository)
        .findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean)
        .getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
            Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
            Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService)
        .insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.SUCCESS);

    Assertions.assertEquals(ITEM_CODE,
        productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }

  @Test
  public void addDeleteVariantRetryPublishListener_WithEmptyItemPickupPointCodeResponseList_Test()
      throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    profileResponse.setCompany(companyDTO);

    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();

    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,
        AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(
            productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(
            productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(
            addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU)))
        .thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "inventoryInsertBatchSizeForEdit", 1);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "pickupPointFetchBatchSizeForReconcilation", 1);

    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(
        addDeleteVariantRetryPublishEventModel);

    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository)
        .findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean)
        .getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
            Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.SUCCESS);

    Assertions.assertEquals(PRODUCT_CODE,
        addDeleteVariantRetryRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void addDeleteVariantRetryPublishListener_WithMixedPreOrderDates_Test() throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    AddDeleteVariantRetryRequest addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
    addDeleteVariantRetryRequest.setProductSku(GDN_PRODUCT_SKU);
    addDeleteVariantRetryRequest.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    profileResponse.setCompany(companyDTO);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    ItemPickupPointCodeResponse itemPickupPointCodeResponse1 = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse1.setItemSku(ITEM_SKU);
    itemPickupPointCodeResponse1.setPickupPointCode(PICKUP_POINT_CODE);
    Date preOrderDate1 = new Date(System.currentTimeMillis() + 86400000);
    itemPickupPointCodeResponse1.setPreOrderDate(preOrderDate1);

    itemPickupPointCodeResponseList.add(itemPickupPointCodeResponse1);

    List<ProductLevel3Inventory> inventoryList = new ArrayList<>();
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setProductSku(GDN_PRODUCT_SKU);
    productLevel3Inventory.setWebItemSku(ITEM_SKU);
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMinAlert(0);
    productLevel3Inventory.setWebMerchantCode(BUSINESS_PARTER_CODE);
    productLevel3Inventory.setWebPickupPointCode(PICKUP_POINT_CODE);
    productLevel3Inventory.setWarehouseItemSku(ITEM_CODE);
    productLevel3Inventory.setWarehouseMerchantCode("BLIBLI-TD");
    productLevel3Inventory.setPreOrderDate(preOrderDate1);
    inventoryList.add(productLevel3Inventory);

    List<ProductLevel3Logistics> productLevel3LogisticsList = new ArrayList<>();
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductCode(PRODUCT_CODE);
    productLevel3LogisticsList.add(productLevel3Logistics);
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    logistics.add(productLevel3Logistics);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,
        AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(
            productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(
            productRepositoryBean.getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID)))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(
            addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU)))
        .thenReturn(itemPickupPointCodeResponseList);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(
            pickupPointOutbound.getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
                Collections.singletonList(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(pickupPointResponse));
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "inventoryInsertBatchSizeForEdit", 1);
    Mockito.doNothing().when(productLevel3InventoryService).insertInventory(inventoryList);
    Mockito.when(
            productLevel3LogisticsService.findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null))
        .thenReturn(productLevel3LogisticsList);
    Mockito.when(
        productLevel3LogisticsService.saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU),
            BUSINESS_PARTER_CODE, logistics, true)).thenReturn(true);
    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "pickupPointFetchBatchSizeForReconcilation", 1);

    addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(
        addDeleteVariantRetryPublishEventModel);

    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository)
        .findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean)
        .getSkuCodesByProductItemIds(Collections.singletonList(ITEM_ID));
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
            Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(GdnMandatoryRequestParameterUtil.getRequestId(),
            Collections.singletonList(PICKUP_POINT_CODE));
    Mockito.verify(productLevel3InventoryService)
        .insertInventory(productLevel3InventoryCaptor.capture());
    Mockito.verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, null);
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Collections.singletonList(ITEM_SKU), BUSINESS_PARTER_CODE,
            logistics, true);
    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.SUCCESS);
    Assertions.assertEquals(ITEM_CODE,
        productLevel3InventoryCaptor.getValue().get(0).getWarehouseItemSku());
  }

  @Test
  public void addDeleteVariantRetryPublishListener_CollectionUtilsConditionFalse_WithNonEmptyPickupPointMap_Test()
      throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        new AddDeleteVariantRetryPublishEventModel();
    addDeleteVariantRetryPublishEventModel.setProductCode(PRODUCT_CODE);
    SimpleStringMapResponse simpleStringMapResponse = new SimpleStringMapResponse();
    Map<String, String> itemIdItemCodeMap = new HashMap<>();
    itemIdItemCodeMap.put(ITEM_ID, ITEM_CODE);
    simpleStringMapResponse.setMapResponse(itemIdItemCodeMap);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setPurchaseTerm("PO");
    companyDTO.setMerchantDeliveryType("REGULAR");
    profileResponse.setCompany(companyDTO);

    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponseList = new ArrayList<>();
    Map<String, List<String>> itemSkuAndPickupPointMap = new HashMap<>();
    itemSkuAndPickupPointMap.put(ITEM_SKU, Arrays.asList(PICKUP_POINT_CODE));

    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    PickupPointResponse pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);
    pickupPointResponse.setFbbActivated(false);
    pickupPointResponseList.add(pickupPointResponse);

    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    Mockito.when(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()).thenReturn("Event");
    Mockito.when(productService.updateAddDeleteVariantStatusForListener(PRODUCT_CODE,
        AddDeleteVariantStatus.IN_PROGRESS)).thenReturn(productCollection);
    Mockito.when(
            productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Collections.singletonList(getProductBusinessPartner()));
    Mockito.when(productRepositoryBean.getSkuCodesByProductItemIds(Collections.emptyList()))
        .thenReturn(simpleStringMapResponse);
    Mockito.when(xProductOutbound.reconcileProductVariants(
            addDeleteVariantRetryRequestArgumentCaptor.capture(), Mockito.eq(GDN_PRODUCT_SKU)))
        .thenReturn(itemPickupPointCodeResponseList); // Returns EMPTY list
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(pickupPointOutbound.getByPickupPointCodes(Mockito.any(),
        Mockito.eq(Arrays.asList(PICKUP_POINT_CODE)))).thenReturn(pickupPointResponseList);

    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "inventoryInsertBatchSizeForEdit", 1);
    ReflectionTestUtils.setField(addDeleteVariantRetryPublishServiceImpl,
        "pickupPointFetchBatchSizeForReconcilation", 1);

    Mockito.when(scheduledJobService.fetchSellerDetailResponse(BUSINESS_PARTER_CODE))
        .thenReturn(new SellerDetailResponse());
    Mockito.when(productService.checkIfProductExistsInPDT(PRODUCT_CODE, false)).thenReturn(true);

    try (MockedStatic<CommonUtils> mockedCommonUtils = Mockito.mockStatic(CommonUtils.class)) {
      mockedCommonUtils.when(
              () -> CommonUtils.getItemSkuAndPickupPointMap(itemPickupPointCodeResponseList))
          .thenReturn(itemSkuAndPickupPointMap);
      mockedCommonUtils.when(() -> CommonUtils.getItemSkuMarkForDeleteMapping(Mockito.any()))
          .thenCallRealMethod();
      mockedCommonUtils.when(
          () -> CommonUtils.setProductLevel3InventoryList(Mockito.any(), Mockito.any(),
              Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(),
              Mockito.anyBoolean(), Mockito.anyBoolean())).thenCallRealMethod();

      addDeleteVariantRetryPublishServiceImpl.processAddDeleteVariantRetryEvents(
          addDeleteVariantRetryPublishEventModel);
    }

    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.IN_PROGRESS);
    Mockito.verify(productBusinessPartnerRepository)
        .findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(productRepositoryBean)
        .getSkuCodesByProductItemIds(Collections.emptyList()); // Changed to empty list
    Mockito.verify(xProductOutbound)
        .reconcileProductVariants(addDeleteVariantRetryRequestArgumentCaptor.capture(),
            Mockito.eq(GDN_PRODUCT_SKU));
    Mockito.verify(businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(BUSINESS_PARTER_CODE);
    Mockito.verify(pickupPointOutbound)
        .getByPickupPointCodes(Mockito.any(), Mockito.eq(Arrays.asList(PICKUP_POINT_CODE)));
    Mockito.verify(productLevel3LogisticsService)
        .findLogisticsByItemSku(ITEM_SKU, BUSINESS_PARTER_CODE, "REGULAR");
    Mockito.verify(productLevel3LogisticsService)
        .saveLogisticsByItemSku(Mockito.any(), Mockito.eq(BUSINESS_PARTER_CODE), Mockito.any(),
            Mockito.eq(true));

    Mockito.verify(scheduledJobService).fetchSellerDetailResponse(BUSINESS_PARTER_CODE);
    Mockito.verify(productService).checkIfProductExistsInPDT(PRODUCT_CODE, false);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productService)
        .updateAddDeleteVariantStatusForListener(PRODUCT_CODE, AddDeleteVariantStatus.SUCCESS);
    Assertions.assertEquals(PRODUCT_CODE,
        addDeleteVariantRetryRequestArgumentCaptor.getValue().getProductCode());
  }
}
