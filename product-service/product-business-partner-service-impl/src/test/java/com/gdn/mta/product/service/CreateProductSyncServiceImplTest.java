package com.gdn.mta.product.service;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author anand
 * @since Sep 2019
 */
public class CreateProductSyncServiceImplTest {

  private static final String PBP_ID = "PBP_ID";

  private static final String STORE_ID = "STORE_ID";

  private static final String USER_NAME = "USER_NAME";

  private static final String NEW_USER_NAME = "NEW_USER_NAME";

  private static final String GDN_ITEM_SKU = "GDN_ITEM_SKU";

  private static final String NEW_GDN_ITEM_SKU = "NEW_GDN_ITEM_SKU";

  private static final String GDN_PRODUCT_SKU = "GDN_PRODUCT_SKU";

  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";

  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";

  private static final String PRODUCT_NAME = "PRODUCT_NAME";

  private static final String CATEGORY_NAME = "CATEGORY_NAME";

  private static final String PRODUCT_BRAND = "PRODUCT_BRAND";

  private static final String PRODUCT_ID = "PRODUCT_ID";

  private static final Integer PRODUCT_TYPE = 1;

  private static final String PRODUCT_ITEM_ID = "PRODUCT_ITEM_ID";

  private static final double PRODUCT_ITEM_PRICE = 1000000000;

  private static final double PRODUCT_ITEM_SALE_PRICE = 345435435;

  private static final Integer PRODUCT_MIN_STOCK = 100;

  private static final Date PRODUCT_ITEM_SALE_START_DATE = new Date(4389325492347L);

  private static final Date PRODUCT_ITEM_SALE_END_DATE = new Date(5489325492347L);

  private static final String ATTRIBUTE_VALUE = "ATTRIBUTE_VALUE";

  private static final String NEW_ATTRIBUTE_VALUE = "NEW_ATTRIBUTE_VALUE";

  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";

  private static final String NEW_ATTRIBUTE_ID = "NEW_ATTRIBUTE_ID";

  private static final String PRODUCT_CODE = "PRODUCT_CODE";

  private static final String ITEM_CODE = "ITEM_CODE";

  private static final String MERCHANT_SKU = "MERCHANT_SKU";

  private static final String PICK_UP_POINT_ID = "PICK_UP_POINT_ID";

  @InjectMocks
  private CreateProductSyncServiceImpl service;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductItemSyncService itemSyncStatusService;

  @Mock
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Mock
  private ProductStatusPublisherService publisherService;

  @Mock
  private ProductGdnSkuGenerator productGdnSkuGenerator;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private ProductInventoryService productInventoryService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @Captor
  private ArgumentCaptor<ProductBusinessPartner> productBusinessPartnerCaptor;

  @Captor
  ArgumentCaptor<ProductCreationRequest> productCreationRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductItemSyncStatus> syncProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemDTO> itemDTOArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductLevel3Inventory>> level3InventoryArgumentCaptor;

  private ProductBusinessPartner productBusinessPartner;
  private ProductBusinessPartnerAttribute productBusinessPartnerAttribute;
  private ProductItemBusinessPartner productItemBusinessPartner;
  private ProfileResponse businessPartnerProfile;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    this.productBusinessPartner = new ProductBusinessPartner();
    this.productBusinessPartner.setProductName(PRODUCT_NAME);
    this.productBusinessPartner.setCategoryName(CATEGORY_NAME);
    this.productBusinessPartner.setBrand(PRODUCT_BRAND);
    this.productBusinessPartner.setProductId(PRODUCT_ID);
    this.productBusinessPartner.setStoreId(STORE_ID);
    this.productBusinessPartner.setGdnProductSku(GDN_PRODUCT_SKU);
    this.productBusinessPartner.setBusinessPartnerId(BUSINESS_PARTNER_CODE);

    this.productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
    this.productBusinessPartnerAttribute.setValue(ATTRIBUTE_VALUE);
    this.productBusinessPartnerAttribute.setStoreId(STORE_ID);
    this.productBusinessPartnerAttribute.setAttributeId(ATTRIBUTE_ID);

    this.productBusinessPartner.setProductBusinessPartnerAttributes(Arrays.asList(this.productBusinessPartnerAttribute));

    this.productItemBusinessPartner = new ProductItemBusinessPartner();
    this.productItemBusinessPartner.setProductType(PRODUCT_TYPE);
    this.productItemBusinessPartner.setProductItemId(PRODUCT_ITEM_ID);
    this.productItemBusinessPartner.setPrice(PRODUCT_ITEM_PRICE);
    this.productItemBusinessPartner.setSalePrice(PRODUCT_ITEM_SALE_PRICE);
    this.productItemBusinessPartner.setSaleStartDate(PRODUCT_ITEM_SALE_START_DATE);
    this.productItemBusinessPartner.setSaleEndDate(PRODUCT_ITEM_SALE_END_DATE);
    this.productItemBusinessPartner.setInstallation(true);
    this.productItemBusinessPartner.setMinimumStock(PRODUCT_MIN_STOCK);
    this.productItemBusinessPartner.setBuyable(true);
    this.productItemBusinessPartner.setDisplay(false);

    this.productItemBusinessPartner.setProductBusinessPartner(this.productBusinessPartner);

    this.businessPartnerProfile = ProfileResponse.builder()
      .merchantStatus(Constants.DEFAULT)
      .company(CompanyDTO.builder().businessPartnerName("CM-NEW-MERCHANT").purchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER).build())
      .build();
    businessPartnerProfile.setId("businessPartnerProfile-id");
  }

  @AfterEach
  public void cleanUp() {
    Mockito.verifyNoMoreInteractions(publisherService);
    Mockito.verifyNoMoreInteractions(itemSyncStatusService);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerService);
    Mockito.verifyNoMoreInteractions(productBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productItemBusinessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productGdnSkuGenerator);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(productImageQcProcessingResponseService);
    Mockito.verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(productLevel3InventoryService);
    Mockito.verifyNoMoreInteractions(productInventoryService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productWorkflowServiceWrapper);
  }

  @Test
  public void createSyncProductTest_whenProductDetailNotFound_thenThrowException() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));

    when(productRepository.findDetailById(PRODUCT_ID)).thenThrow(new Exception());
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
  }

  @Test
  public void createSyncProductTest_whenSingleItemExists() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));

    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName("item-name");
    itemResponse.setSourceItemCode(ITEM_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attributeResponse-id");
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode("attribute-code-1");
    attributeResponse.setName("warna");
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    attributeResponse.setVariantCreation(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue("attribute-value");
    itemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId("image-id-1");
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory("product-story");
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail("specification-detail");
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl("youtube-url");
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode("BRD-01608");
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId("10001");
    productAttributeResponse.setUpdatedBy("user-1");
    productAttributeResponse.setCreatedBy("user-2");
    productAttributeResponse.setProductAttributeName("warna");
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode("allowed-attribute-code");
    allowedAttributeValueResponse.setValue("allowed-attribute-value");
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("predefined-allowed-attribute-value");
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue("descriptive-attribute-value");
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId("category-id");
    category.setCategoryCode("PO-1000038");
    category.setName("Postlive Category 3");
    category.setParentCategoryId("parent-category-id");
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId("41591a5a-daad-11e4-b9d6-1681e6b88ec1");
    catalog.setCatalogCode("10001");
    catalog.setCatalogType("MASTER_CATALOG");
    catalog.setName("MASTER CATALOG");
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Mockito.when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productData);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartnerProfile);
    Mockito.when(productService.generateProductCode()).thenReturn("MTA-00001");
    Mockito.when(productWorkflowServiceWrapper.create(eq(STORE_ID), any(ProductCreationRequest.class),
      Mockito.eq(true), Mockito.eq(false), Mockito.any())).thenReturn(true);
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);
    Mockito.when(publisherService.publishProductSyncSuccessEvent(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(new ProductItemSyncEvent());

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).generateProductCode();
    verify(productWorkflowServiceWrapper)
        .create(eq(STORE_ID), productCreationRequestArgumentCaptor.capture(), Mockito.eq(true), Mockito.eq(false),
            Mockito.any());
    ProductCreationRequest request = productCreationRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    Assertions.assertFalse(request.isActivated());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals("PRODUCT_BRAND", request.getBrand());
    assertEquals(description, request.getDescription());
    assertEquals(10.0, request.getHeight(), 0);
    assertEquals(20.0, request.getLength(), 0);
    assertEquals(description, request.getLongDescription());
    assertEquals("PRODUCT_NAME", request.getName());
    assertEquals("MTA-00001", request.getProductCode());
    assertEquals("product-story", request.getProductStory());
    assertFalse(request.isPromoSKU());
    assertEquals(10.0, request.getShippingWeight(), 0);
    assertEquals("specification-detail", request.getSpecificationDetail());
    assertEquals("Vin 2", request.getUniqueSellingPoint());
    assertEquals("PC", request.getUom());
    assertEquals("youtube-url", request.getUrl());
    assertFalse(request.isViewable());
    assertEquals(10.0, request.getWeight(), 0);
    assertEquals(20.0, request.getWidth(), 0);
    assertEquals("BRD-01608", request.getBrandCode());
    assertEquals("APPROVED", request.getBrandApprovalStatus());
    assertEquals("STORE_ID", request.getStoreId());
    assertEquals(1, request.getProductAttributes().size());
    assertNull(request.getProductAttributes().get(0).getId());
    assertEquals("10001", request.getProductAttributes().get(0).getStoreId());
    assertEquals("warna", request.getProductAttributes().get(0).getProductAttributeName());
    assertEquals(true, request.getProductAttributes().get(0).isOwnByProductItem());
    assertEquals(1, request.getProductAttributes().get(0).getSequence());
    assertEquals("attributeResponse-id", request.getProductAttributes().get(0).getAttribute().getId());
    assertEquals(true, request.getProductAttributes().get(0).getAttribute().isSearchAble());
    assertEquals("attribute-code-1", request.getProductAttributes().get(0).getAttribute().getAttributeCode());
    assertEquals(AttributeType.DESCRIPTIVE_ATTRIBUTE,
      request.getProductAttributes().get(0).getAttribute().getAttributeType());
    assertEquals("allowed-attribute-code",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getAllowedAttributeCode());
    assertEquals("allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getAllowedAttributeValues().get(0).getValue());
    assertEquals("predefined-allowed-attribute-value",
      request.getProductAttributes().get(0).getAttribute().getPredefinedAllowedAttributeValues().get(0).getValue());
    assertEquals("descriptive-attribute-value",
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValue());
    assertEquals(DescriptiveAttributeValueType.SINGLE,
      request.getProductAttributes().get(0).getProductAttributeValues().get(0).getDescriptiveAttributeValueType());
    assertEquals(1, request.getProductCategories().size());
    assertNull(request.getProductCategories().get(0).getId());
    assertEquals("category-id", request.getProductCategories().get(0).getCategory().getId());
    assertEquals("PO-1000038", request.getProductCategories().get(0).getCategory().getCategoryCode());
    assertEquals("parent-category-id", request.getProductCategories().get(0).getCategory().getParentCategory().getId());
    assertEquals("41591a5a-daad-11e4-b9d6-1681e6b88ec1",
      request.getProductCategories().get(0).getCategory().getCatalog().getId());
    assertEquals("10001", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogCode());
    assertEquals("MASTER_CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getCatalogType());
    assertEquals("MASTER CATALOG", request.getProductCategories().get(0).getCategory().getCatalog().getName());
    assertEquals(1, request.getProductBusinessPartnerAttributes().size());
    assertNull(request.getProductBusinessPartnerAttributes().get(0).getId());
    assertEquals("attributeResponse-id", request.getProductBusinessPartnerAttributes().get(0).getAttributeId());
    assertEquals(false, request.getProductBusinessPartnerAttributes().get(0).isMandatory());
    assertEquals("predefined-allowed-attribute-value", request.getProductBusinessPartnerAttributes().get(0).getValue());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("item-name", request.getProductItemRequests().get(0).getItemGeneratedName());
    assertTrue(request.getProductItemRequests().get(0).getAttributesMap().containsKey("attribute-code-1"));
    assertEquals("attribute-value", request.getProductItemRequests().get(0).getAttributesMap().get("attribute-code-1"));
    assertEquals(ITEM_CODE, request.getProductItemRequests().get(0).getSourceItemCode());
    assertNull(request.getProductItemRequests().get(0).getImages().get(0).getId());
    assertNull(request.getProductItemRequests().get(0).getMerchantSku());
    assertTrue(request.getProductItemRequests().get(0).isMarkDefaultAddress());
    assertNull(request.getProductItemRequests().get(0).getGdnProductItemSku());
    assertFalse(request.getProductItemRequests().get(0).isContentChanged());

    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
    verify(publisherService).publishProductSyncSuccessEvent(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void createSyncProductTest_whenFailedToCreateProduct() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));

    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName("item-name");
    itemResponse.setSourceItemCode(ITEM_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue("attribute-value");
    itemResponse.setProductItemAttributeValueResponses(null);

    Image image = new Image();
    image.setId("image-id-1");
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductStory("product-story");
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail("specification-detail");
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl("youtube-url");
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode("BRD-01608");
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId("10001");
    productAttributeResponse.setUpdatedBy("user-1");
    productAttributeResponse.setCreatedBy("user-2");
    productAttributeResponse.setProductAttributeName("warna");
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode("allowed-attribute-code");
    allowedAttributeValueResponse.setValue("allowed-attribute-value");
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("predefined-allowed-attribute-value");
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);
    productData.setProductAttributeResponses(null);

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId("category-id");
    category.setCategoryCode("PO-1000038");
    category.setName("Postlive Category 3");
    category.setParentCategoryId("parent-category-id");
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId("41591a5a-daad-11e4-b9d6-1681e6b88ec1");
    catalog.setCatalogCode("10001");
    catalog.setCatalogType("MASTER_CATALOG");
    catalog.setName("MASTER CATALOG");
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(null);

    Mockito.when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productData);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartnerProfile);
    Mockito.when(productService.generateProductCode()).thenReturn("MTA-00001");
    Mockito.when(productWorkflowServiceWrapper
        .create(eq(STORE_ID), any(ProductCreationRequest.class), Mockito.eq(true), Mockito.eq(false), Mockito.any()))
        .thenReturn(false);
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).generateProductCode();
    verify(productWorkflowServiceWrapper).create(eq(STORE_ID), productCreationRequestArgumentCaptor.capture(),
      Mockito.eq(true), Mockito.eq(false), Mockito.any());
    ProductCreationRequest request = productCreationRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(request);
    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
  }

  @Test
  public void createSyncProductTest_whenBusinessPartnerProfileNotFound_thenThrowException() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setViewable(true);
    when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productDetailResponse);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenThrow(new Exception());
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
  }

  @Test
  public void createSyncProductTest_whenProductNotCreated_thenThrowException() throws Exception {
    productItemBusinessPartner.getProductBusinessPartner().setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));
    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName("item-name");
    itemResponse.setSourceItemCode(ITEM_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue("attribute-value");
    itemResponse.setProductItemAttributeValueResponses(null);

    Image image = new Image();
    image.setId("image-id-1");
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductStory("product-story");
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail("specification-detail");
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl("youtube-url");
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode("BRD-01608");
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId("10001");
    productAttributeResponse.setUpdatedBy("user-1");
    productAttributeResponse.setCreatedBy("user-2");
    productAttributeResponse.setProductAttributeName("warna");
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode("allowed-attribute-code");
    allowedAttributeValueResponse.setValue("allowed-attribute-value");
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("predefined-allowed-attribute-value");
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);
    productData.setProductAttributeResponses(null);

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId("category-id");
    category.setCategoryCode("PO-1000038");
    category.setName("Postlive Category 3");
    category.setParentCategoryId("parent-category-id");
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId("41591a5a-daad-11e4-b9d6-1681e6b88ec1");
    catalog.setCatalogCode("10001");
    catalog.setCatalogType("MASTER_CATALOG");
    catalog.setName("MASTER CATALOG");
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(null);

    Mockito.when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productData);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartnerProfile);
    Mockito.when(productService.generateProductCode()).thenReturn("MTA-00001");
    Mockito.when(productWorkflowServiceWrapper.create(eq(STORE_ID), any(ProductCreationRequest.class),
      Mockito.eq(true), Mockito.eq(false), Mockito.any())).thenThrow(new Exception());
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).generateProductCode();
    verify(productWorkflowServiceWrapper)
        .create(eq(STORE_ID), productCreationRequestArgumentCaptor.capture(), Mockito.eq(true), Mockito.eq(false),
            Mockito.any());
    ProductCreationRequest request = productCreationRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(request);
    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
  }

  @Test
  public void createSyncProductTest_whenProductIsNotViewable_thenThrowException() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setViewable(false);
    when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productDetailResponse);
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
  }

  @Test
  public void createSyncProductTest_whenMultipleAttributeCodeExists_thenThrowException() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));

    ProductItemResponse itemResponse = new ProductItemResponse();
    itemResponse.setId(PRODUCT_ITEM_ID);
    itemResponse.setSkuCode(ITEM_CODE);
    itemResponse.setGeneratedItemName("item-name");
    itemResponse.setSourceItemCode(ITEM_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse_1 = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse_1 = new AttributeResponse();
    attributeResponse_1.setId("attributeResponse-id");
    attributeResponse_1.setSearchAble(true);
    attributeResponse_1.setSkuValue(true);
    attributeResponse_1.setMandatory(false);
    attributeResponse_1.setAttributeCode("attribute-code-1");
    attributeResponse_1.setName("warna");
    attributeResponse_1.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse_1.setBasicView(true);
    attributeResponse_1.setVariantCreation(true);
    productItemAttributeValueResponse_1.setAttributeResponse(attributeResponse_1);
    ProductItemAttributeValueResponse productItemAttributeValueResponse_2 = new ProductItemAttributeValueResponse();
    productItemAttributeValueResponse_2.setValue("attribute-value-2");
    productItemAttributeValueResponse_2.setAttributeResponse(attributeResponse_1);
    productItemAttributeValueResponse_1.setValue("attribute-value");
    itemResponse.setProductItemAttributeValueResponses(
      Arrays.asList(productItemAttributeValueResponse_1, productItemAttributeValueResponse_2));

    Image image = new Image();
    image.setId("image-id-1");
    itemResponse.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory("product-story");
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail("specification-detail");
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl("youtube-url");
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode("BRD-01608");
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);
    productData.setProductItemResponses(Collections.singleton(itemResponse));

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId("10001");
    productAttributeResponse.setUpdatedBy("user-1");
    productAttributeResponse.setCreatedBy("user-2");
    productAttributeResponse.setProductAttributeName("warna");
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode("allowed-attribute-code");
    allowedAttributeValueResponse.setValue("allowed-attribute-value");
    attributeResponse_1.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("predefined-allowed-attribute-value");
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse_1.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse_1);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue("descriptive-attribute-value");
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId("category-id");
    category.setCategoryCode("PO-1000038");
    category.setName("Postlive Category 3");
    category.setParentCategoryId("parent-category-id");
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId("41591a5a-daad-11e4-b9d6-1681e6b88ec1");
    catalog.setCatalogCode("10001");
    catalog.setCatalogType("MASTER_CATALOG");
    catalog.setName("MASTER CATALOG");
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Mockito.when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productData);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(businessPartnerProfile);
    Mockito.when(productService.generateProductCode()).thenReturn("MTA-00001");
    try {
      service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
        Arrays.asList(GDN_ITEM_SKU));
    } catch(Exception ex) {
      verify(productItemBusinessPartnerRepository)
        .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
      verify(productRepository).findDetailById(PRODUCT_ID);
      verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
      verify(productService).generateProductCode();
    }
  }

  @Test
  public void createSyncProductTest_whenExcludeAlreadyCopiedItem() throws Exception {
    Mockito.when(productItemBusinessPartnerRepository
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU)))
      .thenReturn(Arrays.asList(this.productItemBusinessPartner));

    ProductItemResponse itemResponse_1 = new ProductItemResponse();
    itemResponse_1.setId(PRODUCT_ITEM_ID);
    itemResponse_1.setSkuCode(ITEM_CODE);
    itemResponse_1.setGeneratedItemName("item-name");
    itemResponse_1.setSourceItemCode(ITEM_CODE);

    ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setId("attributeResponse-id");
    attributeResponse.setSearchAble(true);
    attributeResponse.setSkuValue(true);
    attributeResponse.setMandatory(false);
    attributeResponse.setAttributeCode("attribute-code-1");
    attributeResponse.setName("warna");
    attributeResponse.setAttributeType("DESCRIPTIVE_ATTRIBUTE");
    attributeResponse.setBasicView(true);
    productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
    productItemAttributeValueResponse.setValue("attribute-value");
    itemResponse_1.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));

    Image image = new Image();
    image.setId("image-id-1");
    itemResponse_1.setImages(Arrays.asList(image));

    ProductDetailResponse productData = new ProductDetailResponse();
    productData.setActivated(true);
    productData.setBrand(PRODUCT_BRAND);
    byte[] description = "PHA+VmluIDE8L3A+".getBytes();
    productData.setDescription(description);
    productData.setHeight(10.0);
    productData.setLength(20.0);
    productData.setLongDescription(description);
    productData.setName(PRODUCT_NAME);
    productData.setProductStory("product-story");
    productData.setPromoSKU(false);
    productData.setShippingWeight(10.0);
    productData.setSpecificationDetail("specification-detail");
    productData.setUniqueSellingPoint("Vin 2");
    productData.setUom("PC");
    productData.setUrl("youtube-url");
    productData.setViewable(true);
    productData.setWeight(10.0);
    productData.setWidth(20.0);
    productData.setBrandCode("BRD-01608");
    productData.setBrandApprovalStatus("APPROVED");
    productData.setStoreId(STORE_ID);
    productData.setProductCode(PRODUCT_CODE);

    ProductItemResponse itemResponse_2 = new ProductItemResponse();
    itemResponse_2.setId("already-copied-product-item-id");
    itemResponse_2.setGeneratedItemName("already-copied-product-item-name");
    HashSet<ProductItemResponse> itemResponseSet=new HashSet<>();
    itemResponseSet.add(itemResponse_1);
    itemResponseSet.add(itemResponse_2);
    productData.setProductItemResponses(itemResponseSet);

    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setId("productAttributeResponse-id");
    productAttributeResponse.setStoreId("10001");
    productAttributeResponse.setUpdatedBy("user-1");
    productAttributeResponse.setCreatedBy("user-2");
    productAttributeResponse.setProductAttributeName("warna");
    productAttributeResponse.setOwnByProductItem(true);
    productAttributeResponse.setSequence(1);

    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setAllowedAttributeCode("allowed-attribute-code");
    allowedAttributeValueResponse.setValue("allowed-attribute-value");
    attributeResponse.setAllowedAttributeValues(Arrays.asList(allowedAttributeValueResponse));

    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse
      = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setValue("predefined-allowed-attribute-value");
    predefinedAllowedAttributeValueResponse.setBrandApprovalStatus("APPROVED");
    attributeResponse.setPredefinedAllowedAttributeValues(Arrays.asList(predefinedAllowedAttributeValueResponse));
    productAttributeResponse.setAttribute(attributeResponse);

    ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
    productAttributeValueResponse.setDescriptiveAttributeValue("descriptive-attribute-value");
    productAttributeValueResponse.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValueResponse);
    productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
    productAttributeResponse.setProductAttributeValues(Arrays.asList(productAttributeValueResponse));
    productData.setProductAttributeResponses(Arrays.asList(productAttributeResponse));

    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setId("9c40c4a0-3fd6-4654-b323-c188062622e9");
    CategoryResponse category = new CategoryResponse();
    category.setId("category-id");
    category.setCategoryCode("PO-1000038");
    category.setName("Postlive Category 3");
    category.setParentCategoryId("parent-category-id");
    CatalogResponse catalog = new CatalogResponse();
    catalog.setId("41591a5a-daad-11e4-b9d6-1681e6b88ec1");
    catalog.setCatalogCode("10001");
    catalog.setCatalogType("MASTER_CATALOG");
    catalog.setName("MASTER CATALOG");
    category.setCatalog(catalog);
    productCategoryResponse.setCategory(category);
    productData.setProductCategoryResponses(Arrays.asList(productCategoryResponse));

    Mockito.when(productRepository.findDetailById(PRODUCT_ID)).thenReturn(productData);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(businessPartnerProfile);
    Mockito.when(productService.generateProductCode()).thenReturn("MTA-00001");
    Mockito.when(productWorkflowServiceWrapper
        .create(eq(STORE_ID), any(ProductCreationRequest.class), eq(true), Mockito.eq(false), Mockito.any())).thenReturn(true);
    Mockito.when(itemSyncStatusService
      .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(ProductItemSyncStatus.builder()
        .businessPartnerCode(BUSINESS_PARTNER_CODE)
        .gdnItemSku(GDN_ITEM_SKU)
        .build()
      );
    Mockito.when(itemSyncStatusService.save(Mockito.any(ProductItemSyncStatus.class)))
      .thenAnswer(mock -> mock.getArguments()[0]);
    Mockito.when(publisherService.publishProductSyncSuccessEvent(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE))
      .thenReturn(new ProductItemSyncEvent());

    service.createSyncProduct(STORE_ID, NEW_USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
      Arrays.asList(GDN_ITEM_SKU));

    verify(productItemBusinessPartnerRepository)
      .findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(STORE_ID, Arrays.asList(GDN_ITEM_SKU));
    verify(productRepository).findDetailById(PRODUCT_ID);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    verify(productService).generateProductCode();
    verify(productWorkflowServiceWrapper)
        .create(eq(STORE_ID), productCreationRequestArgumentCaptor.capture(), eq(true), Mockito.eq(false), Mockito.any());
    ProductCreationRequest request = productCreationRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(request);
    Assertions.assertNull(request.getId());
    assertEquals(1, request.getProductItemRequests().size());
    assertEquals("item-name", request.getProductItemRequests().get(0).getItemGeneratedName());

    verify(itemSyncStatusService).findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
    verify(itemSyncStatusService).save(Mockito.any(ProductItemSyncStatus.class));
    verify(publisherService).publishProductSyncSuccessEvent(STORE_ID, GDN_ITEM_SKU, BUSINESS_PARTNER_CODE);
  }
}
