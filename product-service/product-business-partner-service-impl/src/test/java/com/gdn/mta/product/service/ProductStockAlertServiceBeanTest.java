package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import jakarta.persistence.PersistenceException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.orm.ObjectOptimisticLockingFailureException;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.mta.product.service.util.PbpStockAlertConverterUtil;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailDomainEvent;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.businesspartner.entity.Company;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.entity.ResponsiblePerson;
import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import org.springframework.util.CollectionUtils;

public class ProductStockAlertServiceBeanTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE2 = "BLI-00002";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final Pageable pageable = PageRequest.of(0, 10);
  private static final String DEFAULT_EMAIL = "test@test.com";
  private static final String DEFAULT_MAX_OOS_ATTEMPT = "3";
  private static final Integer DEFAULT_OOS_ATTEMPT = 3;
  private static final String DEFAULT_GDN_SKU = "SKU";
  private static final String DEFAULT_UPDATE_BATCH_SIZE = "10";
  private static final String GDN_SKU_1 = "SKU-1";
  private static final String GDN_SKU_2 = "SKU-2";
  private static final int MAX_STOCK_ALERT_ATTEMPT = 3;
  
  private static final String GDN_SKU = "GDN_SKU";
  private static final String BP_CODE = "BP_CODE";
  private static final String PRD_NAME = "PROD_NAME";
  private static final String ID = "1";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_ID = "product_id";
  private static final String UNIQUE_ID = "UNIQUE_ID";
  private static final int MIN_STOCK = 1;
  private static final int AVL_STOCK = 1;
  private static final String INVENTORY_FULFILLMENT_BLIBLI = "BL";
  private static final String PICKUP_POINT_NAME = "PICKUP_POINT_NAME";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  private static final String DEFAULT_MERCHANT_SKU = "MERCHANT_SKU";
  private static final Double DEFAULT_PRICE = Double.valueOf(0.0);
  private static final Double DEFAULT_SALE_PRICE = Double.valueOf(0.0);
  private static final String OOS_FOLDER_PATH = "oos_stock_update";
  private static final String MIN_STOCK_FOLDER_PATH = "minimum_stock_update";
  private static final String CM_MERCHANT = "CM";
  private static final String NON_CM_MERCHANT = "NON-CM";
  private static final Integer DAYS_TO_CHECK_OOS_FOR = Integer.valueOf(3);
  private static final Long TOTAL_RECORDS = Long.valueOf(3);
  private static final int BATCH_SIZE = 100;

  private PickupPointDTO pickupPointDTO = new PickupPointDTO();
  private List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
  private ProductLevel3Summary productLevel3Summary = new ProductLevel3Summary();
  private CompanyDTO companyDTO = new CompanyDTO();
  private CompanyDTO companyDTO2 = new CompanyDTO();
  private Pageable pageable2 = PageRequest.of(0, 100);
  private ItemViewConfigRequest itemViewConfigRequest = new ItemViewConfigRequest();

  @InjectMocks
  private ProductStockAlertServiceBean productStockAlertServiceBean;

  @Mock
  private ProductStockAlertRepository productStockAlertRepository;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductNotificationService productNotificationService;
  
  @Mock
  private PbpStockAlertConverterUtil pbpStockAlertConverterUtil;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductMailEventPublisher productMailEventPublisher;

  private List<PbpStockAlert> listPbpStockAlert;
  private List<PbpStockAlert> listPbpStockAlert2;
  private List<String> listProductStockAlert;
  private PbpStockAlert pbpStockAlert;
  private PbpStockAlert pbpStockAlert2;
  private Level2InventoryOosEvent level2InventoryOosEvent;
  private Level2InventoryNonOosEvent level2InventoryNonOosEvent;
  private Level2InventoryMinimumStockAlertEvent level2InventoryMinimumStockAlertEvent;
  private ProfileResponse profileResponse= new ProfileResponse();
  private ProfileResponse profileResponse2 = new ProfileResponse();


  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.pbpStockAlert = new PbpStockAlert();
    this.pbpStockAlert.setId(DEFAULT_REQUEST_ID);
    this.pbpStockAlert.setEventTimestamp(new Date(1533389530725L));
    this.pbpStockAlert.setIsMinimumStock(Boolean.FALSE);
    this.pbpStockAlert2 = new PbpStockAlert();
    this.pbpStockAlert2.setId(DEFAULT_REQUEST_ID);
    this.pbpStockAlert2.setGdnSku(DEFAULT_GDN_SKU);
    this.pbpStockAlert2.setEventTimestamp(new Date(1533389530725L));
    this.pbpStockAlert2.setIsMinimumStock(Boolean.TRUE);
    this.pbpStockAlert2.setIsOos(Boolean.FALSE);
    this.pbpStockAlert2.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE2);
    this.pbpStockAlert2.setOosAlertAttempt(DEFAULT_OOS_ATTEMPT);
    this.listPbpStockAlert = new ArrayList<PbpStockAlert>();
    this.listPbpStockAlert.add(this.pbpStockAlert);
    this.listPbpStockAlert.get(0).setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    this.listPbpStockAlert.get(0).setOosAlertAttempt(DEFAULT_OOS_ATTEMPT);
    this.listPbpStockAlert.get(0).setIsOos(true);
    this.listPbpStockAlert.get(0).setGdnSku(DEFAULT_GDN_SKU);
    this.listPbpStockAlert2 = new ArrayList<PbpStockAlert>();
    this.listPbpStockAlert2.add(this.pbpStockAlert2);
    this.listProductStockAlert = new ArrayList<String>();
    this.listProductStockAlert.add(DEFAULT_BUSINESS_PARTNER_CODE);

    Profile businessPartner = new Profile();
    businessPartner.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    businessPartner.setResponsiblePerson(new ResponsiblePerson());
    businessPartner.getResponsiblePerson().setEmail(DEFAULT_EMAIL);
    businessPartner.setMerchantStatus(MerchantStatus.ACTIVE);
    Company company = new Company();
    company.setEmail(DEFAULT_EMAIL);
    company.setMerchantType(CM_MERCHANT);
    company.setOfflineToOnlineFlag(Boolean.FALSE);
    businessPartner.setCompany(company);
    businessPartner.getCompany().setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    Page<PbpStockAlert> pagePbpStockAlert = new PageImpl<PbpStockAlert>(this.listPbpStockAlert);
    Mockito.when(this.productStockAlertRepository.findByOosAlertAttempt(Mockito.anyInt(),
        any(Pageable.class))).thenReturn(pagePbpStockAlert);
    Mockito.when(this.productStockAlertRepository
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT))
        .thenReturn(this.listProductStockAlert);
    Mockito
        .when(this.productStockAlertRepository
            .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(this.listPbpStockAlert);
    Mockito.when(this.productStockAlertRepository
        .findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(this.listProductStockAlert);
    Mockito.when(this.productStockAlertRepository.findById(Mockito.anyString()))
        .thenReturn(Optional.of(this.pbpStockAlert));
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(DEFAULT_MAX_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository.save(Mockito.any(PbpStockAlert.class)))
        .thenReturn(this.pbpStockAlert);
    Mockito
        .when(this.productStockAlertRepository
            .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(DEFAULT_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(DEFAULT_OOS_ATTEMPT);
    Mockito.when(this.applicationProperties.getBatchUpdateSize())
        .thenReturn(DEFAULT_UPDATE_BATCH_SIZE);
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU))
        .thenReturn(listPbpStockAlert);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "system");

    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(MAX_STOCK_ALERT_ATTEMPT + "");

    Mockito.when(this.productMailEventPublisher.publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class)))
        .thenReturn(new ProductMailDomainEvent());
    Mockito.when(this.productMailEventPublisher.publishOOSItemSkuEventEn(Mockito.any(ProductMailDomainEvent.class)))
        .thenReturn(new ProductMailDomainEvent());
    Mockito.when(this.applicationProperties.getMtaUrl()).thenReturn(new String());

    this.level2InventoryMinimumStockAlertEvent =
      new Level2InventoryMinimumStockAlertEvent(STORE_ID, GDN_SKU, AVL_STOCK, MIN_STOCK, BP_CODE,
        PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(1533389530735L);

    this.level2InventoryNonOosEvent =
      new Level2InventoryNonOosEvent(STORE_ID, GDN_SKU, BP_CODE, PICKUP_POINT_CODE, GDN_SKU,
        Boolean.FALSE);
    this.level2InventoryNonOosEvent.setTimestamp(1533389530735L);

    this.level2InventoryOosEvent =
      new Level2InventoryOosEvent(STORE_ID, GDN_SKU, BP_CODE, PICKUP_POINT_CODE, GDN_SKU,
        Boolean.FALSE);
    this.level2InventoryOosEvent.setTimestamp(1533389530735L);

    pickupPointDTO.setName(PICKUP_POINT_NAME);
    pickupPointDTO.setCode(PICKUP_POINT_CODE);
    pickupPointDTOList.add(pickupPointDTO);

    productLevel3Summary.setItemSku(DEFAULT_GDN_SKU);
    productLevel3Summary.setItemName(PRD_NAME);
    productLevel3Summary.setSkuCode(DEFAULT_GDN_SKU);
    productLevel3Summary.setMerchantSku(DEFAULT_MERCHANT_SKU);
    ProductLevel3Price productLevel3Price = new ProductLevel3Price();
    productLevel3Price.setPrice(DEFAULT_PRICE);
    productLevel3Price.setSalePrice(DEFAULT_SALE_PRICE);
    List<ProductLevel3Price> productLevel3PriceList = new ArrayList<>();
    productLevel3PriceList.add(productLevel3Price);
    productLevel3Summary.setPrices(productLevel3PriceList);
    productLevel3Summary.setPickupPointCode(PICKUP_POINT_CODE);
    ProductLevel3ViewConfig productLevel3ViewConfig = new ProductLevel3ViewConfig();
    productLevel3ViewConfig.setBuyable(Boolean.FALSE);
    productLevel3ViewConfig.setDisplay(Boolean.FALSE);
    List<ProductLevel3ViewConfig> productLevel3ViewConfigList = new ArrayList<>();
    productLevel3ViewConfigList.add(productLevel3ViewConfig);
    productLevel3Summary.setViewConfigs(productLevel3ViewConfigList);


    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setResponsiblePerson(new ResponsiblePersonDTO());
    profileResponse.getResponsiblePerson().setEmail(DEFAULT_EMAIL);
    profileResponse.setMerchantStatus(MerchantStatus.ACTIVE.toString());
    companyDTO2.setOfflineToOnlineFlag(Boolean.FALSE);
    companyDTO2.setMerchantType(CM_MERCHANT);
    companyDTO2.setInternationalFlag(Boolean.FALSE);
    profileResponse.setCompany(companyDTO2);
    profileResponse.getCompany().setInventoryFulfillment(INVENTORY_FULFILLMENT_BLIBLI);
    profileResponse.setPickupPoints(pickupPointDTOList);

    profileResponse2.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE2);
    profileResponse2.setResponsiblePerson(new ResponsiblePersonDTO());
    profileResponse2.setCompany(companyDTO);
    profileResponse2.getCompany().setInventoryFulfillment(INVENTORY_FULFILLMENT_BLIBLI);
    profileResponse2.setPickupPoints(pickupPointDTOList);

    itemViewConfigRequest.setDiscoverable(false);
    itemViewConfigRequest.setBuyable(false);

    Mockito.when(this.productLevel3Service.updateItemViewConfigToHideItemSku(itemViewConfigRequest, DEFAULT_GDN_SKU,
            StringUtils.EMPTY))
        .thenReturn(true);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productStockAlertRepository);
    Mockito.verifyNoMoreInteractions(this.businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(this.applicationProperties);
    Mockito.verifyNoMoreInteractions(this.productLevel3InventoryService);
    Mockito.verifyNoMoreInteractions(this.productLevel3Service);
    Mockito.verifyNoMoreInteractions(this.productNotificationService);
    FileUtils.deleteDirectory(new File(OOS_FOLDER_PATH));
    FileUtils.deleteDirectory(new File(MIN_STOCK_FOLDER_PATH));
  }

  @Test
  public void findGdnSkuStockAlertByBusinessPartnerCodeTest() throws Exception {
    this.productStockAlertServiceBean
        .findGdnSkuStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
  }

  @Test
  public void findGdnSkuStockAlertByBusinessPartnerCodeExceptionTest() throws Exception {
    Mockito.doThrow(new PersistenceException()).when(this.productStockAlertRepository)
        .findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    this.productStockAlertServiceBean
        .findGdnSkuStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .findGdnSkuStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
  }

  @Test
  public void findGdnSkuStockAlertTest() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalse(Mockito.anyString()))
        .thenReturn(this.pbpStockAlert);
    this.productStockAlertServiceBean.findPbpStockAlertByGdnSkuCode(DEFAULT_GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalse(Mockito.anyString());
  }

  @Test
  public void createPbpStockAlertTest() throws Exception {
    this.productStockAlertServiceBean.createPbpStockAlert(new PbpStockAlert());
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void createPbpStockAlertExceptionTest() throws Exception {
    Assertions.assertThrows(Exception.class, () -> {
      this.productStockAlertServiceBean.createPbpStockAlert(this.pbpStockAlert);
    });
  }

  @Test
  public void updatePbpStockAlertTest() throws Exception {
    this.productStockAlertServiceBean.updatePbpStockAlert(this.pbpStockAlert);
    Mockito.verify(this.productStockAlertRepository).findById(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updatePbpStockAlertExceptionTest() throws Exception {
    try {
      this.productStockAlertServiceBean.updatePbpStockAlert(new PbpStockAlert());
    } catch (Exception e) {
    }
  }

  @Test
  public void updatePbpStockAlertException2Test() throws Exception {
    Mockito.when(this.productStockAlertRepository.findById(Mockito.anyString())).thenReturn(Optional.empty());
    try {
      this.productStockAlertServiceBean.updatePbpStockAlert(this.pbpStockAlert);
    } catch (Exception e) {
      Mockito.verify(this.productStockAlertRepository).findById(Mockito.anyString());
    }
  }

  @Test
  public void autoBatchUpdateItemViewConfigTest() throws Exception {
    Long totalRowsContent1 = 10L;
    Long totalRowsContent2 = 10L;
    List<PbpStockAlert> contentList1 = new ArrayList<>();
    List<PbpStockAlert> contentList2 = new ArrayList<>();
    for (int i = 0; i < totalRowsContent1; i++) {
      PbpStockAlert content = new PbpStockAlert();
      content.setGdnSku(GDN_SKU_1);
      content.setIsOos(true);
      contentList1.add(content);
    }

    for (int i = 0; i < totalRowsContent2; i++) {
      PbpStockAlert content = new PbpStockAlert();
      content.setGdnSku(GDN_SKU_2);
      content.setIsOos(true);
      contentList2.add(content);
    }


    Pageable firstPageable = PageRequest.of(0, Integer.parseInt(DEFAULT_UPDATE_BATCH_SIZE));
    Pageable lastPageable = PageRequest.of(1, Integer.parseInt(DEFAULT_UPDATE_BATCH_SIZE));
    Page<PbpStockAlert> firstPage = new PageImpl<PbpStockAlert>(contentList1, firstPageable,
        totalRowsContent1 + totalRowsContent2);
    Page<PbpStockAlert> lastPage = new PageImpl<PbpStockAlert>(contentList2,
        firstPage.nextPageable(), totalRowsContent1 + totalRowsContent2);


    // Pageable lastPage = pbpStockAlertPaging.nextPageable();

    Mockito.mock(firstPage.getClass());
    Mockito.mock(lastPage.getClass());
    Mockito.mock(firstPageable.getClass());
    Mockito.mock(lastPageable.getClass());

    ProductLevel3Inventory oosInventory = new ProductLevel3Inventory();
    oosInventory.setWebAvailable(0);

    ProductLevel3Inventory nonOosInventory = new ProductLevel3Inventory();
    nonOosInventory.setWebAvailable(1);

    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    ItemViewConfigDTO itemViewConfigDefault =
        new ItemViewConfigDTO(true, true, ChannelName.DEFAULT.name(), null, null);
    ItemViewConfigDTO itemViewConfigNonDefault =
        new ItemViewConfigDTO(true, true, ChannelName.MOBILE_WEB.name(), null, null);
    itemViewConfigs.add(itemViewConfigDefault);
    itemViewConfigs.add(itemViewConfigNonDefault);

    List<ItemResponse> items = new ArrayList<>();
    ItemResponse item = new ItemResponse();
    item.setItemViewConfigs(itemViewConfigs);
    items.add(item);

    ProductResponse product = new ProductResponse();
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    productLevel3.setProduct(product);
    productLevel3.setItems(items);

    Mockito.when(this.productStockAlertRepository.findByOosAlertAttempt(
            Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1), Mockito.any()))
        .thenReturn(firstPage);
    // Mockito.when(firstPage.nextPageable()).thenReturn(lastPageable);
    Mockito.when(this.productStockAlertRepository.findByOosAlertAttempt(
        Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1),
        Mockito.eq(firstPage.nextPageable()))).thenReturn(lastPage);
    Mockito.when(this.productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.eq(GDN_SKU_1)))
        .thenReturn(oosInventory);
    Mockito.when(this.productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.eq(GDN_SKU_2)))
        .thenReturn(nonOosInventory);
    this.productStockAlertServiceBean.autoBatchUpdateItemViewConfig();
    Mockito.verify(this.applicationProperties).getBatchUpdateSize();
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository, Mockito.times(firstPage.getTotalPages()))
        .findByOosAlertAttempt(Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1),
            Mockito.any());
    Mockito
        .verify(this.productLevel3InventoryService,
            Mockito.times(totalRowsContent1.intValue() + totalRowsContent2.intValue()))
        .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3Service, Mockito.times(totalRowsContent1.intValue()))
        .updateItemViewConfig(Mockito.any(), Mockito.any(), Mockito.anyString());
  }

  @Test
  public void autoBatchUpdateItemViewConfigTest2() throws Exception {
    Long totalRowsContent1 = 10L;
    Long totalRowsContent2 = 10L;
    List<PbpStockAlert> contentList1 = new ArrayList<>();
    List<PbpStockAlert> contentList2 = new ArrayList<>();
    for (int i = 0; i < totalRowsContent1; i++) {
      PbpStockAlert content = new PbpStockAlert();
      content.setGdnSku(GDN_SKU_1);
      content.setIsOos(true);
      contentList1.add(content);
    }

    for (int i = 0; i < totalRowsContent2; i++) {
      PbpStockAlert content = new PbpStockAlert();
      content.setGdnSku(GDN_SKU_2);
      content.setIsOos(true);
      contentList2.add(content);
    }


    Pageable firstPageable = PageRequest.of(0, Integer.parseInt(DEFAULT_UPDATE_BATCH_SIZE));
    Pageable lastPageable = PageRequest.of(1, Integer.parseInt(DEFAULT_UPDATE_BATCH_SIZE));
    Page<PbpStockAlert> firstPage = new PageImpl<PbpStockAlert>(contentList1, firstPageable,
        totalRowsContent1 + totalRowsContent2);
    Page<PbpStockAlert> lastPage = new PageImpl<PbpStockAlert>(contentList2,
        firstPage.nextPageable(), totalRowsContent1 + totalRowsContent2);


    // Pageable lastPage = pbpStockAlertPaging.nextPageable();

    Mockito.mock(firstPage.getClass());
    Mockito.mock(lastPage.getClass());
    Mockito.mock(firstPageable.getClass());
    Mockito.mock(lastPageable.getClass());

    ProductLevel3Inventory oosInventory = new ProductLevel3Inventory();
    oosInventory.setWebSyncStock(true);
    oosInventory.setWarehouseAvailable(0);
    oosInventory.setWebAvailable(0);

    ProductLevel3Inventory nonOosInventory = new ProductLevel3Inventory();
    nonOosInventory.setWebAvailable(1);

    Set<ItemViewConfigDTO> itemViewConfigs = new HashSet<>();
    ItemViewConfigDTO itemViewConfigDefault =
        new ItemViewConfigDTO(true, true, ChannelName.DEFAULT.name(), null, null);
    ItemViewConfigDTO itemViewConfigNonDefault =
        new ItemViewConfigDTO(true, true, ChannelName.MOBILE_WEB.name(), null, null);
    itemViewConfigs.add(itemViewConfigDefault);
    itemViewConfigs.add(itemViewConfigNonDefault);

    List<ItemResponse> items = new ArrayList<>();
    ItemResponse item = new ItemResponse();
    item.setItemViewConfigs(itemViewConfigs);
    items.add(item);

    ProductResponse product = new ProductResponse();
    ProductAndItemsResponse productLevel3 = new ProductAndItemsResponse();
    productLevel3.setProduct(product);
    productLevel3.setItems(items);

    Mockito.when(this.productStockAlertRepository.findByOosAlertAttempt(
            Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1), Mockito.any()))
        .thenReturn(firstPage);
    // Mockito.when(firstPage.nextPageable()).thenReturn(lastPageable);
    Mockito.when(this.productStockAlertRepository.findByOosAlertAttempt(
        Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1),
        Mockito.eq(firstPage.nextPageable()))).thenReturn(lastPage);
    Mockito.when(this.productLevel3InventoryService
            .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.eq(GDN_SKU_1)))
        .thenReturn(oosInventory);
    Mockito.when(this.productLevel3InventoryService
            .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.eq(GDN_SKU_2)))
        .thenReturn(nonOosInventory);
    this.productStockAlertServiceBean.autoBatchUpdateItemViewConfig();
    Mockito.verify(this.applicationProperties).getBatchUpdateSize();
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository, Mockito.times(firstPage.getTotalPages()))
        .findByOosAlertAttempt(Mockito.eq(Integer.parseInt(DEFAULT_MAX_OOS_ATTEMPT) + 1),
            Mockito.any());
    Mockito
        .verify(this.productLevel3InventoryService,
            Mockito.times(totalRowsContent1.intValue() + totalRowsContent2.intValue()))
        .findInventoryByBusinessPartnerCodeAndGdnSku(Mockito.any(), Mockito.any());
    Mockito.verify(this.productLevel3Service, Mockito.times(totalRowsContent1.intValue()))
        .updateItemViewConfig(Mockito.any(), Mockito.any(), Mockito.anyString());
  }

  @Test
  public void autoBatchUpdateItemViewConfigTest_Error() {
    Mockito.doThrow(new RuntimeException("test")).when(this.applicationProperties)
        .getBatchUpdateSize();
    this.productStockAlertServiceBean.autoBatchUpdateItemViewConfig();
    Mockito.verify(this.applicationProperties).getBatchUpdateSize();
  }

  @Test
  public void sendMailAndNotificationIsOosTrueTest() throws Exception {
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 4);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher).publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }

  @Test
  public void sendMailAndNotificationIsOosTrueMerchantStatusInActiveTest() throws Exception {
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 4);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher).publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }



  @Test
  public void sendMailAndNotificationIsOosFalseTest() throws Exception {
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    Mockito
        .when(this.productStockAlertRepository
            .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(this.listPbpStockAlert2);
    Mockito
        .when(this.productStockAlertRepository
            .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(0);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(0);
    Mockito.when(this.productMailEventPublisher.publishMinStockItemSkuEvent(Mockito.any(ProductMailDomainEvent.class)))
        .thenReturn(new ProductMailDomainEvent());

    this.productStockAlertServiceBean.sendMailAndNotification();
    
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher)
        .publishMinStockItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 0);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }

  @Test
  public void sendMailAndNotificationNoBPFoundTest() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT))
        .thenReturn(new ArrayList<String>());
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(1)).getMaxStockAlertAttempt();
  }

  @Test
  public void sendMailAndNotificationTest_Error() throws Exception {
    Mockito.doThrow(new PersistenceException()).when(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(1)).getMaxStockAlertAttempt();
  }

  @Test
  public void findOneGdnSkuStockAlertTest() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.findOnePbpStockAlertByGdnSkuCode(DEFAULT_GDN_SKU);
    Mockito.verify(this.productStockAlertRepository)
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString());
  }

  @Test
  public void findOneGdnSkuStockAlertReturnNullTest() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(null);
    this.productStockAlertServiceBean.findOnePbpStockAlertByGdnSkuCode(DEFAULT_GDN_SKU);
    Mockito.verify(this.productStockAlertRepository)
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString());
  }
  
  @Test
  public void sendMailAndNotificationIsOosTrueWithEmptyWhenGettingByBusinessPartnerCodeTest() throws Exception {
    Mockito.when(
        this.productStockAlertRepository.findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(),
            Mockito.anyInt())).thenReturn(new ArrayList<PbpStockAlert>());
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository).findListBusinessPartnerMinimumStock(
        Mockito.anyInt());
    Mockito.verify(this.applicationProperties, Mockito.times(1)).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository).findPbpStockAlertByBusinessPartnerCode(
        Mockito.anyString(), Mockito.anyInt());
  }
  
  @Test
  public void updateDeletedPbpStockAlertByGdnSkuCodeTest() throws Exception {
    this.productStockAlertServiceBean.updateDeletedPbpStockAlertByGdnSkuCode(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalse(GDN_SKU);
  }
  
  @Test
  public void updateOOSTest() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateOOS_NullEventTimeStampTest() throws Exception {
    this.pbpStockAlert.setGdnSku(GDN_SKU);
    this.pbpStockAlert.setEventTimestamp(null);
    this.pbpStockAlert.setMarkForDelete(Boolean.FALSE);
    this.listPbpStockAlert.add(pbpStockAlert);
    this.level2InventoryOosEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryNonOosEvent.setLevel2Id(GDN_SKU);
    this.level2InventoryNonOosEvent.setTimestamp(
      new Date().getTime() - TimeUnit.SECONDS.toSeconds(20));
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU))
      .thenReturn(Collections.singletonList(this.pbpStockAlert));
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertEquals(PICKUP_POINT_CODE, pbpStockAlert.getPickupPointCode());

  }
  @Test
  public void updateNonOOSTest() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }
  @Test
  public void updateNonOOSTimeStampTest() throws Exception{
    this.pbpStockAlert.setGdnSku(GDN_SKU);
    this.pbpStockAlert.setEventTimestamp(null);
    this.pbpStockAlert.setMarkForDelete(Boolean.FALSE);
    this.listPbpStockAlert.add(pbpStockAlert);
    this.level2InventoryNonOosEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryNonOosEvent.setLevel2Id(GDN_SKU);
    this.level2InventoryNonOosEvent.setTimestamp(
      new Date().getTime() - TimeUnit.SECONDS.toSeconds(20));
    Mockito.when(this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertEquals(PICKUP_POINT_CODE, pbpStockAlert.getPickupPointCode());
  }

  @Test
  public void updateNonOOSnonNullTimeStampTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(GDN_SKU);
    pbpStockAlert.setEventTimestamp(new Date());
    level2InventoryNonOosEvent.setPickupPointCode(PICKUP_POINT_CODE);
    level2InventoryNonOosEvent.setLevel2Id(GDN_SKU);
    level2InventoryNonOosEvent.setTimestamp(new Date().getTime() - TimeUnit.SECONDS.toSeconds(30));
    listPbpStockAlert.add(pbpStockAlert);
    Mockito.when(this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(
        Mockito.anyString())).thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository)
      .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertEquals(PICKUP_POINT_CODE, listPbpStockAlert.get(0).getPickupPointCode());
  }

  @Test
  public void updateNonOOSNullStockAlertTest() throws Exception {
    PbpStockAlert pbpStockAlert = null;
    level2InventoryNonOosEvent.setPickupPointCode(PICKUP_POINT_CODE);
    level2InventoryNonOosEvent.setLevel2Id(GDN_SKU);
    Mockito.when(this.productStockAlertRepository.findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(
        Mockito.anyString())).thenReturn(Collections.singletonList(pbpStockAlert));
      this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository)
      .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
  }
  @Test
  public void updateMinimumStockTest() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateMinimumStockOOSTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(GDN_SKU);
    pbpStockAlert.setEventTimestamp(new Date());
    level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    level2InventoryMinimumStockAlertEvent.setAvailableStock(0);
    level2InventoryMinimumStockAlertEvent.setTimestamp(new Date().getTime());
    listPbpStockAlert.add(pbpStockAlert);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(Collections.singletonList(pbpStockAlert));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertNotNull(pbpStockAlert.getOosDate());
    Assertions.assertEquals(PICKUP_POINT_CODE, pbpStockAlert.getPickupPointCode());
  }

  @Test
  public void updateMinimumStockNonOOSTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(GDN_SKU);
    pbpStockAlert.setEventTimestamp(new Date());
    pbpStockAlert.setIsOos(Boolean.FALSE);
    level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    level2InventoryMinimumStockAlertEvent.setAvailableStock(Integer.MAX_VALUE);
    level2InventoryMinimumStockAlertEvent.setTimestamp(new Date().getTime());
    listPbpStockAlert.add(pbpStockAlert);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(Collections.singletonList(pbpStockAlert));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertNull(pbpStockAlert.getOosDate());
    Assertions.assertEquals(0, (int) pbpStockAlert.getOosAlertAttempt());
    Assertions.assertEquals(PICKUP_POINT_CODE, pbpStockAlert.getPickupPointCode());
  }

  @Test
  public void updateMinimumStockTimeStampNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(GDN_SKU);
    pbpStockAlert.setEventTimestamp(null);
    pbpStockAlert.setIsOos(Boolean.FALSE);
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(Integer.MAX_VALUE);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date().getTime());
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(Collections.singletonList(pbpStockAlert));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertEquals(level2InventoryMinimumStockAlertEvent.getTimestamp(),
      pbpStockAlert.getEventTimestamp().getTime());
    Assertions.assertEquals(PICKUP_POINT_CODE, pbpStockAlert.getPickupPointCode());
  }

  @Test
  public void updateMinimumStockNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(0);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date(1533389530725L).getTime());
    this.level2InventoryMinimumStockAlertEvent.setBusinessPartnerCode(BP_CODE);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(null);
    Mockito.when(this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyInt(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(Date.class),
      Mockito.anyString())).thenReturn(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    verify(this.pbpStockAlertConverterUtil).converterNewPbpStockAlert(STORE_ID, BP_CODE, GDN_SKU,
      PRD_NAME, 0, MIN_STOCK, true, true, new Date(1533389530725L), PICKUP_POINT_CODE);
  }
  @Test
  public void updateMinimumStockNotOOSNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(AVL_STOCK);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date(1533389530725L).getTime());
    this.level2InventoryMinimumStockAlertEvent.setBusinessPartnerCode(BP_CODE);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(null);
    Mockito.when(this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyInt(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(Date.class),
      Mockito.anyString())).thenReturn(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    verify(this.pbpStockAlertConverterUtil).converterNewPbpStockAlert(STORE_ID, BP_CODE, GDN_SKU,
      PRD_NAME, AVL_STOCK, MIN_STOCK, true,
      level2InventoryMinimumStockAlertEvent.getAvailableStock() == 0, new Date(1533389530725L),
      PICKUP_POINT_CODE);
  }
  @Test
  public void updateNonMinimumStockTest() throws Exception {
    Mockito
    .when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateNonMinimumStockTimeStampNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(GDN_SKU);
    pbpStockAlert.setEventTimestamp(null);
    pbpStockAlert.setIsOos(Boolean.FALSE);
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(Integer.MAX_VALUE);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date().getTime());
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(Collections.singletonList(pbpStockAlert));
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
    Assertions.assertEquals(level2InventoryMinimumStockAlertEvent.getTimestamp(),
      pbpStockAlert.getEventTimestamp().getTime());
  }

  @Test
  public void updateNonMinimumStockNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(0);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date(1533389530725L).getTime());
    this.level2InventoryMinimumStockAlertEvent.setBusinessPartnerCode(BP_CODE);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(null);
    Mockito.when(this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyInt(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(Date.class),
      Mockito.anyString())).thenReturn(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    verify(this.pbpStockAlertConverterUtil).converterNewPbpStockAlert(STORE_ID, BP_CODE, GDN_SKU,
      PRD_NAME, 0, MIN_STOCK, false, level2InventoryMinimumStockAlertEvent.getAvailableStock() == 0,
      new Date(1533389530725L), PICKUP_POINT_CODE);
  }

  @Test
  public void updateNonMinimumStockNonOOSNullTest() throws Exception {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    this.level2InventoryMinimumStockAlertEvent.setPickupPointCode(PICKUP_POINT_CODE);
    this.level2InventoryMinimumStockAlertEvent.setGdnSku(GDN_SKU);
    this.level2InventoryMinimumStockAlertEvent.setAvailableStock(AVL_STOCK);
    this.level2InventoryMinimumStockAlertEvent.setTimestamp(new Date(1533389530725L).getTime());
    this.level2InventoryMinimumStockAlertEvent.setBusinessPartnerCode(BP_CODE);
    Mockito.when(
        this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
      .thenReturn(null);
    Mockito.when(this.pbpStockAlertConverterUtil.converterNewPbpStockAlert(Mockito.anyString(),
      Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(),
      Mockito.anyInt(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(Date.class),
      Mockito.anyString())).thenReturn(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent,
      PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    verify(this.pbpStockAlertConverterUtil).converterNewPbpStockAlert(STORE_ID, BP_CODE, GDN_SKU,
      PRD_NAME, AVL_STOCK, MIN_STOCK, false,
      level2InventoryMinimumStockAlertEvent.getAvailableStock() == 0, new Date(1533389530725L),
      PICKUP_POINT_CODE);
  }
  @Test
  public void archiveGdnSkuTest() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, true, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void unArchiveGdnSkuTest() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, false, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateOOS_new_Test() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU)).thenReturn(null);
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }
  
  @Test
  public void updateNonOOS_new_Test() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU)).thenReturn(null);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
  }
  
  @Test
  public void updateMinimumStock_new_Test() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU))
        .thenReturn(null);
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }
  
  @Test
  public void updateNonMinimumStock_new_Test() throws Exception {
    Mockito.when(this.productStockAlertRepository.findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU))
        .thenReturn(null);
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }
  
  @Test
  public void archiveGdnSku_new_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, true, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void unArchiveGdnSku_new_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, false, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateOOS_exception_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateNonOOS_exception_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateMinimumStock_exception_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void archiveGdnSku_exception_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, true, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void unArchiveGdnSku_exception_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, false, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateNonMinimumStock_exception_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(RuntimeException.class).when(this.productStockAlertRepository)
        .save(Mockito.any());
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateOOS_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateNonOOS_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateMinimumStock_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void updateNonMinimumStock_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
    .when(this.productStockAlertRepository
        .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
    .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void archiveGdnSku_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, true, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }
  
  @Test
  public void unArchiveGdnSku_objectOptimisticLockingFailureObjectOptimisticLockingFailureException_Test() throws Exception {
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    Mockito.doThrow(ObjectOptimisticLockingFailureException.class).when(this.productStockAlertRepository)
        .save(Mockito.any(PbpStockAlert.class));
    this.productStockAlertServiceBean.archiveGdnSku(GDN_SKU, false, 0);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(4)).findByGdnSkuAndMarkForDeleteTrueOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository, times(3)).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void sendMailAndNotificationTest_merchantBliBliFullfilled() throws Exception {
    profileResponse.getCompany().setInventoryFulfillment(INVENTORY_FULFILLMENT_BLIBLI);
    productLevel3Summary.setAvailableStockLevel1(AVL_STOCK);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 4);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher).publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }

  @Test
  public void sendMailAndNotification_expectExceptionTest() throws Exception {
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenThrow(Exception.class);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    try {
      this.productStockAlertServiceBean.sendMailAndNotification();
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.productStockAlertRepository).findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
      Mockito.verify(this.applicationProperties, Mockito.times(2)).getMaxStockAlertAttempt();
      Mockito.verify(this.productStockAlertRepository)
          .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
      Mockito.verify(this.productStockAlertRepository).countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
      Mockito.verify(this.productStockAlertRepository)
          .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
      Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
      Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 4);
      Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
      Mockito.verify(this.productNotificationService).sendProductStockOosNotification(Mockito.anyString(), Mockito.anyInt());
      Mockito.verify(this.productNotificationService).sendProductStockMinimalNotification(Mockito.anyString(), Mockito.anyInt());
      Mockito.verify(this.productMailEventPublisher).publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
      Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
    }
  }

  @Test
  public void sendMailAndNotificationIsOosTrueTest_internationalMerchat() throws Exception {
    profileResponse.getCompany().setInternationalFlag(Boolean.TRUE);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 4);
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher).publishOOSItemSkuEventEn(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }

  @Test
  public void sendMailAndNotificationIsOosFalseTest_internationalMerchant() throws Exception {
    profileResponse.getCompany().setInternationalFlag(Boolean.TRUE);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel()).thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    Mockito
        .when(this.productStockAlertRepository
            .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(this.listPbpStockAlert2);
    Mockito
        .when(this.productStockAlertRepository
            .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(0);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(0);
    Mockito.when(this.productMailEventPublisher.publishMinStockItemSkuEventEn(Mockito.any(ProductMailDomainEvent.class)))
        .thenReturn(new ProductMailDomainEvent());

    this.productStockAlertServiceBean.sendMailAndNotification();

    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties, Mockito.times(2))
        .getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.productMailEventPublisher)
        .publishMinStockItemSkuEventEn(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.productStockAlertRepository).updateOosAlertAttempt(DEFAULT_GDN_SKU, 0);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
  }

  @Test
  public void sendMailAndNotificationIsOosTrueMerchantStatusInactiveTest() throws Exception {
    profileResponse.setMerchantStatus(MerchantStatus.INACTIVE.toString());
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository).findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
  }

  @Test
  public void sendMailAndNotification_nonCMMerchantTest() throws Exception {
    profileResponse.getCompany().setMerchantType(NON_CM_MERCHANT);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository).findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
  }

  @Test
  public void sendMailAndNotification_O2OMerchantTrueTest() throws Exception {
    profileResponse.getCompany().setOfflineToOnlineFlag(Boolean.TRUE);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(profileResponse);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
  }

  @Test
  public void sendMailAndNotification_nullBusinessPartnerTest() throws Exception {
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(null);
    this.productStockAlertServiceBean.sendMailAndNotification();
    Mockito.verify(this.productStockAlertRepository).findListBusinessPartnerMinimumStock(MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(Mockito.anyString(), Mockito.anyInt());
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
  }

  @Test
  public void updateOOSTest_stockAlertMFDTrue() throws Exception {
    listPbpStockAlert.get(0).setMarkForDelete(Boolean.TRUE);
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateNonOOSTest_stockAlertMFDTrue() throws Exception {
    listPbpStockAlert.get(0).setMarkForDelete(Boolean.TRUE);
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateMinimumStockTest_stockAlertMFDTrue() throws Exception {
    listPbpStockAlert.get(0).setMarkForDelete(Boolean.TRUE);
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }

  @Test
  public void updateNonMinimumStockTest_stockAlertMFDTrue() throws Exception {
    listPbpStockAlert.get(0).setMarkForDelete(Boolean.TRUE);
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
    Mockito.verify(this.productStockAlertRepository).save(Mockito.any(PbpStockAlert.class));
  }


  @Test
  public void updateOOSTest_noUpdate() throws Exception {
    listPbpStockAlert.get(0).setEventTimestamp(new Date());
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateOOS(level2InventoryOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }

  @Test
  public void updateNonOOSTest_noUpdate() throws Exception {
    listPbpStockAlert.get(0).setEventTimestamp(new Date());
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonOOS(level2InventoryNonOosEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuAndMarkForDeleteFalseOrderByUpdatedDateDesc(GDN_SKU);
  }

  @Test
  public void updateMinimumStockTest_noUpdate() throws Exception {
    listPbpStockAlert.get(0).setEventTimestamp(new Date());
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }

  @Test
  public void updateNonMinimumStockTest_noUpdate() throws Exception {
    listPbpStockAlert.get(0).setEventTimestamp(new Date());
    Mockito
        .when(this.productStockAlertRepository
            .findByGdnSkuOrderByUpdatedDateDesc(Mockito.anyString()))
        .thenReturn(this.listPbpStockAlert);
    this.productStockAlertServiceBean.updateNonMinimumStock(level2InventoryMinimumStockAlertEvent, PRD_NAME, 0);
    Mockito.verify(this.productStockAlertRepository).findByGdnSkuOrderByUpdatedDateDesc(GDN_SKU);
  }


  @Test
  public void hideItemSkuByOOSDateTest() throws Exception {
    Mockito.when(this.applicationProperties.getDaysToCheckOOSDateFor()).thenReturn(DAYS_TO_CHECK_OOS_FOR);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2)))
        .thenReturn(new PageImpl<>(listPbpStockAlert, pageable, TOTAL_RECORDS));
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    this.productStockAlertServiceBean.hideItemSkuByOOSDate(STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
    Mockito.verify(this.applicationProperties).getDaysToCheckOOSDateFor();
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3Service)
        .updateItemViewConfigToHideItemSku(itemViewConfigRequest, DEFAULT_GDN_SKU, StringUtils.EMPTY);
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2));
  }

  @Test
  public void hideItemSkuByOOSDateTest_expectExceptionOnItemViewConfigUpdate() throws Exception {
    Mockito.when(this.applicationProperties.getDaysToCheckOOSDateFor()).thenReturn(DAYS_TO_CHECK_OOS_FOR);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2)))
        .thenReturn(new PageImpl<>(listPbpStockAlert, pageable, TOTAL_RECORDS));
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    doThrow(Exception.class).when(this.productLevel3Service).updateItemViewConfig(itemViewConfigRequest, DEFAULT_GDN_SKU,
        StringUtils.EMPTY);
    this.productStockAlertServiceBean.hideItemSkuByOOSDate(STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
    Mockito.verify(this.applicationProperties).getDaysToCheckOOSDateFor();
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productLevel3Service).updateItemViewConfigToHideItemSku(itemViewConfigRequest, DEFAULT_GDN_SKU,
        StringUtils.EMPTY);
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2));
  }

  @Test
  public void hideItemSkuByOOSDateTest_emptyStockAlerts() throws Exception {
    Mockito.when(this.applicationProperties.getDaysToCheckOOSDateFor()).thenReturn(DAYS_TO_CHECK_OOS_FOR);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2)))
        .thenReturn(null);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    this.productStockAlertServiceBean.hideItemSkuByOOSDate(STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
    Mockito.verify(this.applicationProperties).getDaysToCheckOOSDateFor();
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2));
  }

  @Test
  public void hideItemSkuByOOSDateTest_nonCMMerchantTest() throws Exception {
    profileResponse.getCompany().setMerchantType(NON_CM_MERCHANT);
    Mockito.when(this.applicationProperties.getDaysToCheckOOSDateFor()).thenReturn(DAYS_TO_CHECK_OOS_FOR);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2)))
        .thenReturn(new PageImpl<>(listPbpStockAlert, pageable, TOTAL_RECORDS));
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    this.productStockAlertServiceBean.hideItemSkuByOOSDate(STORE_ID, DEFAULT_REQUEST_ID, BATCH_SIZE);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.applicationProperties).getDaysToCheckOOSDateFor();
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndOosDateLessThanAndMarkForDeleteFalseOrderByOosDate(eq(STORE_ID),
            Mockito.any(Date.class), eq(pageable2));
  }

  @Test
  public void updateStockAlertSendMailAndNotification_offlineProductTest() throws Exception {
    productLevel3Summary.getViewConfigs().get(0).setBuyable(Boolean.FALSE);
    productLevel3Summary.getViewConfigs().get(0).setDisplay(Boolean.FALSE);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(DEFAULT_MAX_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(1);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_OOS_ATTEMPT)).thenReturn(1);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel())
        .thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean
        .sendMailAndNotificationForStockAlert(Arrays.asList(pbpStockAlert), profileResponse);
    Mockito.verify(this.productMailEventPublisher)
        .publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
  }

  @Test
  public void updateStockAlertSendMailAndNotification_onlineProductTest() throws Exception {
    productLevel3Summary.getViewConfigs().get(0).setBuyable(Boolean.TRUE);
    productLevel3Summary.getViewConfigs().get(0).setDisplay(Boolean.TRUE);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(DEFAULT_MAX_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(1);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_OOS_ATTEMPT)).thenReturn(1);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel())
        .thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean
        .sendMailAndNotificationForStockAlert(Arrays.asList(pbpStockAlert), profileResponse);
    Mockito.verify(this.productMailEventPublisher)
        .publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
  }

  @Test
  public void updateStockAlertSendMailAndNotification_B2BProductTest() throws Exception {
    productLevel3Summary.getViewConfigs().get(0).setBuyable(Boolean.TRUE);
    productLevel3Summary.getViewConfigs().get(0).setDisplay(Boolean.FALSE);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(DEFAULT_MAX_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(1);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_OOS_ATTEMPT)).thenReturn(1);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel())
        .thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean
        .sendMailAndNotificationForStockAlert(Arrays.asList(pbpStockAlert), profileResponse);
    Mockito.verify(this.productMailEventPublisher)
        .publishOOSItemSkuEvent(Mockito.any(ProductMailDomainEvent.class));
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getDirectoryStockAlertsExcel();
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productLevel3Service).findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU);
  }

  @Test
  public void updateStockAlertSendMailAndNotificationTest_nonOOS() throws Exception {
    pbpStockAlert.setIsOos(false);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(DEFAULT_MAX_OOS_ATTEMPT);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(1);
    Mockito.when(this.productStockAlertRepository
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_OOS_ATTEMPT)).thenReturn(1);
    Mockito.when(this.applicationProperties.getDirectoryStockAlertsExcel())
        .thenReturn(StringUtils.EMPTY);
    Mockito.when(this.productLevel3Service.findSummaryByGdnSku(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_GDN_SKU))
        .thenReturn(productLevel3Summary);
    this.productStockAlertServiceBean
        .sendMailAndNotificationForStockAlert(Arrays.asList(pbpStockAlert), profileResponse);
    Mockito.verify(this.productNotificationService)
        .sendProductStockMinimalNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productNotificationService)
        .sendProductStockOosNotification(DEFAULT_BUSINESS_PARTNER_CODE, 1);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithMinimumStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertRepository)
        .countGdnSkuWithOosStockAlertByBusinessPartnerCode(eq(DEFAULT_BUSINESS_PARTNER_CODE),
            Mockito.anyInt());
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
  }

  @Test
  public void findListBusinessPartnerMinimumStockTest() throws Exception {
    Mockito.when(
        this.productStockAlertRepository.findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    List<String> response =
        productStockAlertServiceBean.findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT);
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, response.get(0));
  }

  @Test
  public void findListBusinessPartnerMinimumStockTest_throwException() throws Exception {
    Mockito.when(
        this.productStockAlertRepository.findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT))
        .thenThrow(new PersistenceException());
    List<String> response =
        productStockAlertServiceBean.findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT);
    Mockito.verify(this.productStockAlertRepository)
        .findListBusinessPartnerMinimumStock(DEFAULT_OOS_ATTEMPT);
    Assertions.assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void findPbpStockAlertByBusinessPartnerCodeTest() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_OOS_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    List<PbpStockAlert> result = productStockAlertServiceBean
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE,
            MAX_STOCK_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertRepository)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_OOS_ATTEMPT);
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE, result.get(0).getBusinessPartnerCode());
    Assertions.assertEquals(DEFAULT_GDN_SKU, result.get(0).getGdnSku());
  }

  @Test
  public void updateStockAlertForMailerTest() throws Exception {
    productStockAlertServiceBean.updateStockAlertForMailer(pbpStockAlert);
    Mockito.verify(this.productStockAlertRepository)
        .updateOosAlertAttemptById(pbpStockAlert.getId(), pbpStockAlert.getOosAlertAttempt() + 1);
  }

  @Test
  public void deleteProductStockAlertByStoreIdAndProductIdTest() {
    productStockAlertServiceBean.deleteProductStockAlertByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(productStockAlertRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

}
