package com.gdn.partners.pbp.service.productlevel1;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
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

import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.BrandRepository;
import com.gdn.mta.product.repository.BrandWipRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.service.ItemService;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.entity.Product;
import org.slf4j.MDC;

public class ProductLevel1WipServiceTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String DEFAULT_CATEGORY_CODE = "BLI-00001";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0353468";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";
  private static final String BRAND = "Brand";
  private static final String IN_REVIEW_BRAND = "Brand (IN_REVIEW)";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_REQUEST_CODE = "brandRequestCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String DRAFT_STATUS = "DRAFT";
  public static final String BULK_CLIENT_ID = "x-bulk";
  private static final String bulkUploadType = "UNIFIED_BULK_UPLOAD";
  private static final String ADDITIONAL_NOTES = "additionalNotes";
  private static final String VENDOR_ERROR_NOTES = "correctionReason";
  private static final List<String> ERROR_FIELDS = Arrays.asList("Description");
  private static final String USERNAME = "username";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String STORE_ID = "store-id";

  private ScreeningProductBulkActionsRequest screeningProductBulkActionsRequest;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ProductLevel1SolrService productLevel1SolrService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;
  
  @Mock
  private ProductWorkflowRepository oldProductWorkflowRepository;

  @Mock
  private BrandRepository brandRepository;

  @Mock
  private BrandWipRepository brandWipRepository;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private ProductLevel1WipServiceBean productLevel1WipServiceBean;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>> configurationStatusRequestArgumentCaptor;

  private ProductCollection productCollection;
  private BrandResponse brandResponse;
  private List<ConfigurationStatusResponse> configurationStatusResponses;
  private BrandWipResponse brandWipResponse;

  private ProductRequest generateProductRequest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    productRequest.setBrand(BRAND);
    productRequest.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    productRequest.setProductCode(PRODUCT_CODE);
    List<ProductAttributeRequest> productAttributeRequests = new ArrayList<>();
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setProductAttributeName(BRAND);

    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(BRAND);
    productAttributeRequest.setAttribute(attributeRequest);

    List<ProductAttributeValueRequest> productAttributeValueRequests = new ArrayList<>();
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(BRAND_CODE);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productAttributeValueRequests.add(productAttributeValueRequest);

    productAttributeRequest.setProductAttributeValues(productAttributeValueRequests);
    productAttributeRequests.add(productAttributeRequest);

    productRequest.setProductAttributes(productAttributeRequests);

    ProductCategoryRequest productCategory = new ProductCategoryRequest();
    CategoryRequest category = new CategoryRequest();
    category.setCategoryCode(DEFAULT_CATEGORY_CODE);
    productCategory.setCategory(category);
    productRequest.setProductCategories(Arrays.asList(productCategory));
    return productRequest;
  }

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    return productCollection;
  }

  private Product generateProduct() throws Exception {
    Product product = new Product();
    return product;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
   productCollection = this.generateProductCollection();
    Product product = this.generateProduct();
    brandResponse = new BrandResponse();
    brandResponse.setBrandName(BRAND);
    brandResponse.setBrandCode(BRAND_CODE);
    brandResponse.setProtectedBrand(Boolean.TRUE);
    brandWipResponse = new BrandWipResponse();
    brandWipResponse.setBrandName(IN_REVIEW_BRAND);
    brandWipResponse.setBrandCode(BRAND_CODE);
    brandWipResponse.setBrandRequestCode(BRAND_REQUEST_CODE);

    screeningProductBulkActionsRequest = new ScreeningProductBulkActionsRequest();
    screeningProductBulkActionsRequest.setProductCodes(Arrays.asList(DEFAULT_PRODUCT_CODE));
    screeningProductBulkActionsRequest.setAllVariants(true);
    screeningProductBulkActionsRequest.setVendorErrorFields(ERROR_FIELDS);
    screeningProductBulkActionsRequest.setVendorNotes(Arrays.asList(VENDOR_ERROR_NOTES));
    screeningProductBulkActionsRequest.setImageReason(Arrays.asList(VENDOR_ERROR_NOTES));
    screeningProductBulkActionsRequest.setContentAdditionalNotes(ADDITIONAL_NOTES);
    screeningProductBulkActionsRequest.setImagesAdditionalNotes(ADDITIONAL_NOTES);

    Mockito.doNothing().when(this.productRepository).create(Mockito.any(ProductRequest.class));
    Mockito.doNothing().when(this.productLevel1CollectionService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(BRAND_APPROVAL_STATUS), eq(false), eq(bulkUploadType));
    Mockito.doNothing().when(this.productLevel1SolrService).update(Mockito.anyString());
    Mockito.doNothing().when(this.productRepository).updateActivated(Mockito.anyString(), Mockito.anyBoolean());
    Mockito.doReturn(productCollection).when(this.productLevel1CollectionService).approveDraft(Mockito.anyString(),
        Mockito.any());
    Mockito.doNothing().when(this.productLevel1CollectionService).update(Mockito.anyString());
    Mockito.doNothing().when(this.productLevel1CollectionService).activate(Mockito.anyString());
    Mockito.doNothing().when(this.productRepository).updateViewable(Mockito.anyString(), Mockito.anyBoolean());
    Mockito.when(
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            Mockito.anyString())).thenReturn(productCollection);
    Mockito.when(this.productRepository.findOne(Mockito.anyString())).thenReturn(product);
    Mockito.doNothing().when(this.productRepository).delete(Mockito.any(ProductDetailResponse.class));
    Mockito.doNothing().when(this.productLevel1CollectionService).delete(Mockito.anyString());
    Mockito.when(this.oldProductWorkflowRepository.save(Mockito.any(ProductWorkflow.class))).thenReturn(null);
    ConfigurationStatusResponse configurationStatusResponse = new ConfigurationStatusResponse();
    configurationStatusResponse.setCategoryCode(DEFAULT_CATEGORY_CODE);
    configurationStatusResponse.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    configurationStatusResponse.setReviewConfig("Post-Live");
    configurationStatusResponses = Arrays.asList(configurationStatusResponse);
    Mockito.doNothing().when(this.itemService).publishItemStatusEvent(null, ProductStatus.ACTIVE);

    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getChannelId()).thenReturn(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getClientId()).thenReturn(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getUsername()).thenReturn(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);

  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.productLevel1CollectionService);
    Mockito.verifyNoMoreInteractions(this.productLevel1SolrService);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.brandRepository);
    Mockito.verifyNoMoreInteractions(this.brandWipRepository);
    Mockito.verifyNoMoreInteractions(this.itemService);
  }

  @Test
  public void createTest() throws Exception {
    ProductRequest request = this.generateProductRequest();
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true))
        .thenReturn(new GdnRestSingleResponse<>(brandResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    this.productLevel1WipServiceBean.create(ProductLevel1WipServiceTest.DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType, request);
    Mockito.verify(this.pcbFeign).filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true);
    Mockito.verify(this.productRepository).create(Mockito.any(ProductRequest.class));
    Mockito.verify(this.productLevel1CollectionService)
        .create(Mockito.any(), Mockito.any(), (String) Mockito.any(), Mockito.any(), Mockito.any(), eq(true), eq(bulkUploadType));
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(this.productOutbound)
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void createTest_expectException() throws Exception {
    ProductRequest request = this.generateProductRequest();
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true)).thenReturn(null);
    Mockito.when(
        this.brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(BRAND, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    try {
      this.productLevel1WipServiceBean.create(ProductLevel1WipServiceTest.DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType, request);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.pcbFeign)
          .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true);
      Mockito.verify(this.brandWipRepository)
          .findBrandWipByBrandNameAndBusinessPartnerCode(BRAND, DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void createDirectTest() throws Exception {
    ProductRequest request = this.generateProductRequest();
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true))
        .thenReturn(new GdnRestSingleResponse<>(brandResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    this.productLevel1WipServiceBean.create(null, null, bulkUploadType, request);
    Mockito.verify(this.pcbFeign).filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true);
    Mockito.verify(this.productRepository).create(Mockito.any(ProductRequest.class));
    Mockito.verify(this.productLevel1CollectionService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(BRAND_APPROVAL_STATUS), eq(true), eq(bulkUploadType));
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
  }

  @Test
  public void createWithInReviewApprovedStatusBrandTest() throws Exception {
    ProductRequest request = this.generateProductRequest();
    request.setBrand(BRAND);
    request.setBrandApprovalStatus(BRAND_APPROVAL_STATUS);
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true))
        .thenReturn(new GdnRestSingleResponse<>(brandResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(new SimpleBooleanResponse(true));
    this.productLevel1WipServiceBean.create(ProductLevel1WipServiceTest.DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType, request);
    Mockito.verify(this.pcbFeign).filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true);
    Mockito.verify(this.productRepository).create(Mockito.any(ProductRequest.class));
    Mockito.verify(this.productLevel1CollectionService)
        .create(Mockito.any(), Mockito.any(), (String) Mockito.any(), Mockito.any(), Mockito.any(), eq(true), eq(bulkUploadType));
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
    Mockito.verify(this.productOutbound)
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void createDirectDraftNameTest() throws Exception {
    ProductRequest request = this.generateProductRequest();
    Mockito.when(this.pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true)).thenReturn(null);
    Mockito.when(
        this.brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(BRAND, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(brandWipResponse);
    Mockito.when(this.productRepository.getConfigurationStatus(Mockito.anyList()))
        .thenReturn(configurationStatusResponses);
    this.productLevel1WipServiceBean.create(DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType,request);
    Mockito.verify(this.pcbFeign).filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, BRAND, false, true);
    Mockito.verify(this.brandWipRepository)
        .findBrandWipByBrandNameAndBusinessPartnerCode(BRAND, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productRepository).create(request);
    Mockito.verify(this.productLevel1CollectionService)
        .create(DEFAULT_BUSINESS_PARTNER_CODE, null, PRODUCT_CODE, BRAND_REQUEST_CODE, DRAFT_STATUS, true, bulkUploadType);
    Mockito.verify(this.productRepository).getConfigurationStatus(configurationStatusRequestArgumentCaptor.capture());
  }

  @Test
  public void approveDraftTest() throws Exception {
    this.productLevel1WipServiceBean.approveDraft(null, null);
    Mockito.verify(this.productRepository).updateActivated(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productLevel1CollectionService).approveDraft(Mockito.any(), Mockito.any());

    Mockito.verify(this.itemService).publishItemStatusEvent(null, ProductStatus.ACTIVE);
  }

  @Test
  public void approveContentTest() throws Exception {
    this.productLevel1WipServiceBean.approveContent(null);
    Mockito.verify(this.productLevel1CollectionService).update(Mockito.any());

  }

  @Test
  public void approveImageTest() throws Exception {
    this.productLevel1WipServiceBean.approveImage(null);

  }

  @Test
  public void activateTest() throws Exception {
    this.productLevel1WipServiceBean.activate(null);
    Mockito.verify(this.productLevel1CollectionService).activate(Mockito.any());
    Mockito.verify(this.productRepository).updateViewable(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productLevel1SolrService).update(Mockito.any());

  }

  @Test
  public void deleteTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    Mockito.when(productRepository.findDetailById(Mockito.any())).thenReturn(productDetailResponse);
    this.productLevel1WipServiceBean.delete(DEFAULT_PRODUCT_CODE, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.any(), eq(DEFAULT_PRODUCT_CODE));
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.productRepository).delete(Mockito.any());
    Mockito.verify(this.productLevel1CollectionService).delete((ProductCollection) Mockito.any());
    Mockito.verify(this.oldProductWorkflowRepository).save(Mockito.any());
  }

  @Test
  public void deleteActiveProductTest() throws Exception {
    ProductDetailResponse productDetailResponse = getProductDetailResponse();
    productCollection.setViewable(Boolean.TRUE);
    Mockito.when(productRepository.findDetailById(Mockito.any())).thenReturn(productDetailResponse);
    this.productLevel1WipServiceBean.delete(DEFAULT_PRODUCT_CODE, null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.any(), eq(DEFAULT_PRODUCT_CODE));
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.productRepository).delete(Mockito.any());
    Mockito.verify(this.productLevel1SolrService).deleteByProductCollectionId(Mockito.any());
    Mockito.verify(this.productLevel1CollectionService).delete((ProductCollection) Mockito.any());
    Mockito.verify(this.oldProductWorkflowRepository).save(Mockito.any());
  }

  @Test
  public void deleteTest_alreadyDeleted() throws Exception {
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            eq(DEFAULT_PRODUCT_CODE))).thenReturn(null);
    this.productLevel1WipServiceBean.delete(DEFAULT_PRODUCT_CODE, null);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.anyString(),
            eq(DEFAULT_PRODUCT_CODE));
  }

  @Test
  public void returnDraftForCorrectionTest() throws Exception {
    this.productLevel1WipServiceBean
        .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, false, true);
    verify(this.productLevel1CollectionService)
        .returnDraftForCorrection(DEFAULT_PRODUCT_CODE, screeningProductBulkActionsRequest, false, false, true);
  }

  @Test
  public void updateRejectedProductTest() throws Exception {
    this.productLevel1WipServiceBean.updateRejectedProduct(DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productLevel1CollectionService).updateRejectedProduct(Mockito.anyString());
  }

  @Test
  public void resubmitTest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductCode(ProductLevel1WipServiceTest.DEFAULT_PRODUCT_CODE);
    this.productLevel1WipServiceBean.resubmit(productRequest, new Date());
    Mockito.verify(this.productOutbound).updateProductContent(Mockito.any(), Mockito.anyBoolean());
    Mockito.verify(this.productRepository).updateProductImage(Mockito.any());
    Mockito.verify(this.productLevel1CollectionService).resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
  }

  @Test
  public void validateProtectedBrand_pbpExceptionTest() throws Exception {
    SimpleBooleanResponse simpleBooleanResponse = new SimpleBooleanResponse(false);
    ProductRequest request = this.generateProductRequest();
    Mockito.when(this.pcbFeign
      .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND, false, true))
      .thenReturn(new GdnRestSingleResponse<>(brandResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(simpleBooleanResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel1WipServiceBean
            .create(DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType, request);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND, false, true);
      Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
          DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void validateProtectedBrandExceptionTest() throws Exception {
    SimpleBooleanResponse simpleBooleanResponse = new SimpleBooleanResponse(false);
    ProductRequest request = this.generateProductRequest();;
    Mockito.when(this.pcbFeign
      .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        BRAND, false, true))
      .thenReturn(new GdnRestSingleResponse<>(brandResponse, Constants.DEFAULT_REQUEST_ID));
    Mockito.when(this.productOutbound
      .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
        DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(simpleBooleanResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productLevel1WipServiceBean
            .create(DEFAULT_BUSINESS_PARTNER_CODE, null, bulkUploadType, request);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
          BRAND, false, true);
      Mockito.verify(this.productOutbound)
        .authoriseProtectedBrand(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, brandResponse.getBrandCode(),
          DEFAULT_BUSINESS_PARTNER_CODE);
    }
  }


  private ProductDetailResponse getProductDetailResponse() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductCode(null);
    productDetailResponse.setName("Produk 1");
    productDetailResponse.setImages(Arrays.asList(new Image()));
    productDetailResponse.setId(UUID.randomUUID().toString());
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setImages(Arrays.asList(new Image()));
    Set<ProductItemResponse> productItemResponses = new HashSet<>();
    productItemResponses.add(productItemResponse);
    productDetailResponse.setProductItemResponses(productItemResponses);
    ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
    productAttributeResponse.setAttribute(new AttributeResponse());
    productDetailResponse.setProductAttributeResponses(Arrays.asList(productAttributeResponse));
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(new CategoryResponse());
    productDetailResponse.setProductCategoryResponses(Arrays.asList(productCategoryResponse));
    productDetailResponse.setStoreId(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    return productDetailResponse;
  }
}
