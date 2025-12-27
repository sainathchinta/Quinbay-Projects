package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;

import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.EditedResizeAndImagesUpdateStatusResponse;
import com.gda.mta.product.dto.ItemErrorListResponse;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.OrderCancellationDto;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

import java.util.List;

import org.mockito.ArgumentCaptor;
import org.mockito.Captor;

public class ProductLevel3V2ServiceWrapperBeanTest {

  public static final String PRODUCT_ID = "productId";
  public static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String RESTRICTED_KEYWORD = "restrictedKeyword";
  public static final String PARENT_CATEGORY_ID = "parentCategoryId";
  @InjectMocks
  private ProductLevel3V2ServiceWrapperBean productLevel3V2ServiceWrapperBean;

  @Mock
  private ProductLevel3V2Service productLevel3V2Service;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private VariantEditValidationService variantEditValidationService;

  @Mock
  private ProductLevel3Helper productLevel3Helper;

  @Mock
  private ProductService productService;

  @Mock
  private ValidationUtil validationUtil;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Captor
  private ArgumentCaptor<ProductMasterDataEditRequest> productMasterDataEditRequestArgumentCaptor;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductPublisherService productPublisherService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";
  private static final String USER_NAME = "userName";
  private static final String BUSINESS_PARTNER_CODE = "DD6-44662";
  private static final String FAMILY_COLOUR_ATTRIBUTE_CODE = "FA-2000060";
  private static final String ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  private static final String ATTRIBUTE_VALUE_1 = "ATTRIBUTE_VALUE_1";
  private static final String ATTRIBUTE_VALUE_2 = "ATTRIBUTE_VALUE_2";
  private static final String ATTRIBUTE_ID = "ATTRIBUTE_ID";
  private static final Double LENGTH = 1.0;
  private static final Double WIDTH = 1.0;
  private static final Double HEIGHT = 1.0;
  private static final Double WEIGHT = 1.0;
  private static final Double SHIPPING_WEIGHT = 0.1;
  private static final String DESCRIPTION = "description";
  private static final String LONG_PRODUCT_NAME =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut sit amet placerat erat. "
      + "Pellentesque mi turpis, suscipit at pellentesque sit amet tincidunt.";
  private static final String FINAL_PATH = "final-image/";

  private OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
  private ProductEditValidationDTO productEditValidationDTO = new ProductEditValidationDTO();
  private ProfileResponse profileResponse = new ProfileResponse();
  private ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
  private EditProductResponse editProductResponse = new EditProductResponse();
  private ProductL3Response productL3Response = new ProductL3Response();
  private ProductLevel3 productLevel3 = new ProductLevel3();
  private ProductCollection productCollection;
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest;
  private MasterDataProductDTO masterDataProductDTO;
  private MasterDataAttributeDTO masterDataAttributeDTO;
  private MasterDataProductAttributeDTO masterDataProductAttributeDTO;
  private static final String GENERATED_ITEM_NAME_4 = "Item Blue L";
  private ProductMasterDataEditRequest productMasterDataEditRequest;
  private static final String INVALID_SIZE_CHART_CODE = "invalidSizeChartCode";
  private static final String PRODUCT_NAME = "Iphone 15";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String NEW_CATEGORY_CODE = "newCategoryCode";
  private static final String NEW_CATEGORY_NAME = "newCategoryName";



  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    orderCancellationDto.setOrderItemStatus(Constants.ORDER_ITEM_STATUS_OFF);
    orderCancellationDto.setItemGdnSku(ITEM_SKU);
    orderCancellationDto.setPickupPointCode(PICKUP_POINT_CODE);
    orderCancellationDto.setProductGdnSku(PRODUCT_SKU);
    orderCancellationDto.setStoreId(STORE_ID);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productL3UpdateRequest.setPreOrder(PreOrderRequest.builder().isPreOrder(true).build());
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productL3Response.setStoreId(STORE_ID);
    productLevel3.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productEditValidationDTO.setProductL3Response(productL3Response);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    productEditValidationDTO.setProductL3UpdateRequest(productL3UpdateRequest);
    productEditValidationDTO.setProductLevel3(productLevel3);
    productVariantPriceStockAndImagesRequest = new ProductVariantPriceStockAndImagesRequest();
    productL3UpdateRequest.setProductItems(
        new ArrayList<>(Collections.singleton(productVariantPriceStockAndImagesRequest)));
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", false);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "familyColorAttributeCode",
        FAMILY_COLOUR_ATTRIBUTE_CODE);
    masterDataProductDTO = new MasterDataProductDTO();
    productL3Response.setMasterDataProduct(masterDataProductDTO);
    masterDataAttributeDTO = new MasterDataAttributeDTO();
    masterDataAttributeDTO.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttributeDTO.setId(ATTRIBUTE_ID);
    masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO.setMasterDataAttribute(masterDataAttributeDTO);
    masterDataProductDTO.setMasterDataProductAttributes(
        new ArrayList<>(Collections.singletonList(masterDataProductAttributeDTO)));
    productMasterDataEditRequest =
      ProductMasterDataEditRequest.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU)
        .productName(PRODUCT_NAME).description(DESCRIPTION).productType(1).instore(false)
        .b2cActivated(false).weight(WEIGHT).length(LENGTH).height(HEIGHT).width(WIDTH).categoryCode(CATEGORY_CODE).categoryName(CATEGORY_CODE)
        .shippingWeight(SHIPPING_WEIGHT).masterDataEditChangeTypes(Collections.emptySet()).build();
    productMasterDataEditRequest.setUpdatedBy(USER_NAME);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "productNameCharacterLimit", 10);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "maxProductDimensionLimit",
      100);
    Mockito.when(variantEditValidationService.validateBundleProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setMarkForDelete(false);
    productCollection.setActivated(true);
    productCollection.setProductId(PRODUCT_ID);
    Mockito.when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE)).thenReturn(productCollection);
    Mockito.doNothing().when(productLevel3Helper).autoFillFamilyColourAttribute(Mockito.any());
    Mockito.when(productLevel3ServiceBean.processAutoApproval(productCollection, CATEGORY_CODE,
      USER_NAME)).thenReturn(Pair.of(AutoApprovalType.NA, getCategoryResponses()));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productLevel3V2Service,xProductOutbound);
  }

  private List<CategoryResponse> getCategoryResponses() {
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setParentCategoryId(null);
    categoryResponse1.setInternalActivationInterval(24);
    CategoryResponse categoryResponse2 = new CategoryResponse();
    categoryResponse2.setParentCategoryId(PARENT_CATEGORY_ID);
    return Arrays.asList(categoryResponse1, categoryResponse2);
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.AUTO_CANCEL);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
    }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationSpanNullTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.AUTO_CANCEL);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());

  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationWithApiErrorCodeTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(ApiErrorCode.ITEM_PICKUP_POINT_NOT_FOUND);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.AUTO_CANCEL);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationWithErrorListTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(Arrays.asList(itemErrorListResponse));
    orderCancellationDto.setCancellationActor(Constants.AUTO_CANCEL);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationWithApiErrorCodeAndErrorListTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(ApiErrorCode.ITEM_PICKUP_POINT_NOT_FOUND);
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(Arrays.asList(itemErrorListResponse));
    orderCancellationDto.setCancellationActor(Constants.AUTO_CANCEL);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorSellerTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.SELLER);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorMTATest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorOrderCenterTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.ORDER_CENTER);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorOFFTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.CANCELLATION_ACTOR_OFF);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorDefaultTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.ACTIVE);
    Mockito.when(productLevel3V2Service.quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any()))
        .thenReturn(itemPriceStockQuickUpdateResponse);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(productLevel3V2Service)
        .quickEditPatching(Mockito.eq(STORE_ID), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationOrderItemStatusInvalidTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    orderCancellationDto.setOrderItemStatus(Constants.ORDER_CANCELLATION);
    productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Assertions.assertNull(orderCancellationDto.getStatusOFFUpdatedBy());
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationOrderItemStatusEmptyTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    orderCancellationDto.setOrderItemStatus(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    });
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationOrderItemSkuEmptyTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    orderCancellationDto.setItemGdnSku(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    });
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationPickupPointEmptyTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    orderCancellationDto.setPickupPointCode(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    });
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationProductSkuEmptyTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(Constants.MTA_API);
    orderCancellationDto.setProductGdnSku(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    });
  }

  @Test
  public void updateL4AndL5ToOfflineForOrderCancellationActorEmptyTest() throws Exception {
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    orderCancellationDto.setCancellationActor(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productLevel3V2ServiceWrapperBean.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    });
  }

  @Test
  public void validationForEditRequest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "validateLogisticErrorBeforeL5Update", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Assertions.assertNotNull(productEditValidationDTO.getProductL3UpdateRequest().getPreOrder());
  }

  @Test
  public void validationForEditRequest_updateInstoreFlagEditRequestBasedOnSellerTrueAndsetDefaultB2CActivated() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "validateLogisticErrorBeforeL5Update", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "updateInstoreFlagEditRequestBasedOnSeller", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "setDefaultB2CActivated", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
                Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Assertions.assertNotNull(productEditValidationDTO.getProductL3UpdateRequest().getPreOrder());
  }

  @Test
  public void validationForEditRequestValidationOff() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "validateLogisticErrorBeforeL5Update", false);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
                Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Assertions.assertNotNull(productEditValidationDTO.getProductL3UpdateRequest().getPreOrder());
  }

  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }


  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnOverrideItemNameTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "overrideItemNameGeneration", true);
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setProductName(PRODUCT_NAME);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    productVariantPriceStockAndImagesRequest.setItemName(GENERATED_ITEM_NAME_4);
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setItemName(GENERATED_ITEM_NAME_4));
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setNewlyAddedItem(true));
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),Mockito.any(),
          Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku()))
      .thenReturn(productL3Response);
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.eq(productL3Response), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku());
  }

  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnOverrideItemNameNoNewVariantTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "overrideItemNameGeneration", true);
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(false);
    productVariantPriceStockAndImagesRequest.setItemName(GENERATED_ITEM_NAME_4);
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setItemName(GENERATED_ITEM_NAME_4));
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setNewlyAddedItem(false));
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(), Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku()))
      .thenReturn(productL3Response);
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }
  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnEmptyNameTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    productL3UpdateRequest.setProductName(PRODUCT_NAME);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.eq(productL3Response), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku()))
        .thenReturn(productL3Response);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.any(), Mockito.eq(profileResponse));
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku());
  }

  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnAttributeMapNotEmptyTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "overrideItemNameGeneration", true);
    productL3UpdateRequest.setProductName(PRODUCT_NAME);
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    Map<String, String> attributeCodeAndId = new HashMap<>();
    attributeCodeAndId.put(ATTRIBUTE_CODE, ATTRIBUTE_ID);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setItemName(GENERATED_ITEM_NAME_4));
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(ATTRIBUTE_ID, ATTRIBUTE_VALUE_2);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class), Mockito.any(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.when(productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku()))
        .thenReturn(productL3Response);
    Mockito.when(productLevel3V2Service.getAttributeCodeAndIdMap(eq(true), any())).thenReturn(attributeCodeAndId);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.any(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku());
    Mockito.verify(productLevel3V2Service).getAttributeCodeAndIdMap(eq(true), any());
  }

  @Test
  public void validationForEditRequestGenerateItemNameSwitchOnAttributeMapNotSwitchOnOverrideNameEmptyTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "overrideItemNameGeneration",
      false);
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "generateItemNameEnabled", true);
    productVariantPriceStockAndImagesRequest.setItemName(GENERATED_ITEM_NAME_4);
    productL3UpdateRequest.setProductSku(PRODUCT_SKU);
    Map<String, String> attributeCodeAndId = new HashMap<>();
    attributeCodeAndId.put(ATTRIBUTE_CODE, ATTRIBUTE_ID);
    productVariantPriceStockAndImagesRequest.setNewlyAddedItem(true);
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(ATTRIBUTE_ID, ATTRIBUTE_VALUE_2);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    editProductResponse.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO.setEditProductResponse(editProductResponse);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(), Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    Mockito.when(productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku()))
      .thenReturn(productL3Response);
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).getAttributeCodeAndIdMap(eq(true), any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }

  @Test
  public void validationForEditRequestProductEditValidationNullTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse.setApiErrorCode(null);
    editProductResponse1.setApiErrorCode(ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setNeedCorrection(true);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(),  Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(),Mockito.any(), Mockito.anyBoolean(),  Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }

  @Test
  public void validationForEditRequestProductEditValidationUpdateEditInfoTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(false);
    productL3UpdateRequest.getCommonImages().forEach(
      productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
        "OLD"));
    productL3UpdateRequest.getProductItems().forEach(
      productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.getImages()
        .forEach(
          productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
            "NEW")));
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    editProductResponse1.setToTakeDown(false);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(false);
    itemsPriceStockImagesUpdateResponse.setTakeDown(true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
            Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).takeDownProduct(Mockito.any(),Mockito.any(),Mockito.any());
  }

  @Test
  public void validationForEditRequestIsProductReviewTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    editProductResponse1.setToTakeDown(false);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    productL3UpdateRequest.getCommonImages().forEach(
      productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
        "OLD"));
    productL3UpdateRequest.getProductItems().forEach(
      productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.getImages()
        .forEach(
          productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
            "NEW")));
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setTakeDown(false);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
            Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }

  @Test
  public void validationForEditRequestEditResponseTakenDownTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    editProductResponse.setAction(1);
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    editProductResponse.setProfileResponse(profileResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    ProductLevel3Logistics productLevel3Logistics = new ProductLevel3Logistics();
    productLevel3Logistics.setLogisticProductName(PRODUCT_SKU);
    productL3UpdateRequest.setProductLevel3LogisticsRequest(Arrays.asList(productLevel3Logistics));
    productEditValidationDTO1.setProductL3UpdateRequest(productL3UpdateRequest);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(false);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
        new EditedResizeAndImagesUpdateStatusResponse());
    itemsPriceStockImagesUpdateResponse.setTakeDown(true);
    itemsPriceStockImagesUpdateResponse.setReviewType("boom");
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    doNothing().when(productServiceWrapper).publishEditedImageResizeEvent(Mockito.anyString(),
        Mockito.any(EditedResizeAndImagesUpdateStatusResponse.class));
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(),Mockito.any(), Mockito.anyBoolean(),  Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
            Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).takeDownProduct(Mockito.any(),Mockito.any(),Mockito.any());
  }

  @Test
  public void validationForEditRequestProductEditValidationUpdateEditInfoErrorTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(false);
    editProductResponse.setProfileResponse(new ProfileResponse());
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    EditProductResponse editProductResponse2 = new EditProductResponse();
    editProductResponse2.setApiErrorCode(ApiErrorCode.FREE_SAMPLE_CNC_UPDATE_ERROR);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(),Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse2);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(itemsPriceStockImagesUpdateResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false, false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(),Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
  }

  @Test
  public void validationForEditRequestProductEditValidationUpdateEditInfoErrorValidationOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "validateLogisticErrorBeforeL5Update", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(false);
    editProductResponse.setProfileResponse(new ProfileResponse());
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    EditProductResponse editProductResponse2 = new EditProductResponse();
    editProductResponse2.setApiErrorCode(ApiErrorCode.FREE_SAMPLE_CNC_UPDATE_ERROR);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
                Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse2);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(itemsPriceStockImagesUpdateResponse);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false, false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
  }


  @Test
  public void validationForEditRequestAutoRejectCombinedEditTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "combinedEditFlowEnabled", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    EditProductResponse editProductResponse2 = new EditProductResponse();
    editProductResponse2.setAction(3);
    editProductResponse2.setProfileResponse(profileResponse);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse2);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
      .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
        Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service).updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(any(), any());
  }

  @Test
  public void validationForEditRequestAutoRejectTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    EditProductResponse editProductResponse2 = new EditProductResponse();
    editProductResponse2.setAction(3);
    editProductResponse2.setProfileResponse(profileResponse);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse2);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(),Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
  }


  @Test
  public void validationForEditRequestAutoNeedRevisionPublishEventTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    ProductDetailEditDTO productDetailEditDTO = new ProductDetailEditDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    editProductResponse.setAction(2);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    editProductResponse.setProfileResponse(profileResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setAuditTrailDtoList(Collections.singletonList(new AuditTrailDto()));
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    editProductResponse.setProductDetailEditDTO(productDetailEditDTO);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
        new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    doNothing().when(productServiceWrapper).publishEditedImageResizeEvent(Mockito.anyString(),
        Mockito.any(EditedResizeAndImagesUpdateStatusResponse.class));
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
            Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .publishProductLevelHistoryToPcbForDistributionUpdate(Mockito.any(), Mockito.any(), Mockito.anyList());
  }

  @Test
  public void validationForEditRequestAutoNeedRevisionPublishEventWithCombinedEditEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "combinedEditFlowEnabled", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    editProductResponse.setProductReview(true);
    editProductResponse.setAction(2);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    editProductResponse.setProfileResponse(profileResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    when(productLevel3V2Service.performProductDetailEdit(anyBoolean(), anyBoolean(), anyBoolean(),
      any(EditProductResponse.class), any(ProductEditValidationDTO.class),
      any(ProfileResponse.class))).thenReturn(new ProductEditValidationDTO());
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(),Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
      Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class),
      Mockito.eq(false), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(),Mockito.anyBoolean())).thenReturn(
      productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
      Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
      new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
      Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
        productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
          Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
      .thenReturn(itemsPriceStockImagesUpdateResponse);
    doNothing().when(productServiceWrapper).publishEditedImageResizeEvent(Mockito.anyString(),
      Mockito.any(EditedResizeAndImagesUpdateStatusResponse.class));
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(), Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
      .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service)
      .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
        Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
      Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
      .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
      .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
        Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
  }

  @Test
  public void validationForEditRequestlogisticUpdateResponseErrorTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productL3UpdateRequest.setCommonImages(Collections.singletonList(
      ProductLevel3SummaryDetailsImageRequest.builder().build()));
    productL3UpdateRequest.getCommonImages().forEach(
      productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
        "NEW"));
    productL3UpdateRequest.getProductItems().forEach(
      productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.getImages()
        .forEach(
          productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
            "OLD")));
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    editProductResponse.setProductReview(true);
    editProductResponse1.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setTakeDown(false);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
        new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    doNothing().when(productServiceWrapper).publishEditedImageResizeEvent(Mockito.anyString(),
        Mockito.any(EditedResizeAndImagesUpdateStatusResponse.class));
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(),Mockito.any(), Mockito.anyBoolean(),  Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
            Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void validationForEditRequestlogisticUpdateValidationSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "validateLogisticErrorBeforeL5Update", true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productL3UpdateRequest.setCommonImages(Collections.singletonList(
        ProductLevel3SummaryDetailsImageRequest.builder().build()));
    productL3UpdateRequest.getCommonImages().forEach(
        productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
            "NEW"));
    productL3UpdateRequest.getProductItems().forEach(
        productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.getImages()
            .forEach(
                productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
                    "OLD")));
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    editProductResponse.setProductReview(true);
    editProductResponse1.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
                Mockito.isNull(), Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setTakeDown(false);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
        new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    Mockito.when(
            productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.anyBoolean(),
                Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean()))
        .thenReturn(ApiErrorCode.INVALID_DATA_INPUT);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
        .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
            Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean());
  }

  @Test
  public void validationForEditRequestBundleErrorTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    editProductResponse.setProductReview(true);
    editProductResponse1.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
        Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.eq(false))).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
        Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
        Mockito.any(ProductL3Response.class), Mockito.anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setTakeDown(false);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
        new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
        Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
            productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
                Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
        .thenReturn(itemsPriceStockImagesUpdateResponse);
    Mockito.when(
            productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.anyBoolean(),
                Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean()))
        .thenReturn(ApiErrorCode.INVALID_DATA_INPUT);
    ProductEditValidationDTO productEditValidationDTO2 = new ProductEditValidationDTO();
    productEditValidationDTO2.setEditProductResponse(editProductResponse);
    productEditValidationDTO2.getEditProductResponse().setApiErrorCode(ApiErrorCode.ALL_VARIANTS_ERROR);
    Mockito.when(variantEditValidationService.validateBundleProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productEditValidationDTO2);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }

  @Test
  public void validationForEditRequestlogisticUpdateResponseErrorNoNewImageTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    productL3UpdateRequest.setCommonImages(Collections.singletonList(
      ProductLevel3SummaryDetailsImageRequest.builder().build()));
    productL3UpdateRequest.getCommonImages().forEach(
      productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
        "OLD"));
    productL3UpdateRequest.getProductItems().forEach(productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.setImages(Collections.singletonList(ProductLevel3SummaryDetailsImageRequest.builder()
      .build())));
    productL3UpdateRequest.getProductItems().forEach(
      productVariantPriceStockAndImagesRequest1 -> productVariantPriceStockAndImagesRequest1.getImages()
        .forEach(
          productLevel3SummaryDetailsImageRequest -> productLevel3SummaryDetailsImageRequest.setReviewType(
            "NEW")));
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    editProductResponse.setProductReview(true);
    editProductResponse1.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(),Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(ProductLevel3.class),
      Mockito.any(EditProductResponse.class), Mockito.any(ProfileResponse.class), Mockito.eq(false),
      any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO1);
    Mockito.when(productLevel3V2Service.updateEditInfo(Mockito.any(ProductLevel3.class), Mockito.anyBoolean(),
      Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(ProfileResponse.class),
      Mockito.any(ProductL3Response.class), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(editProductResponse);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    itemsPriceStockImagesUpdateResponse.setProductReview(true);
    itemsPriceStockImagesUpdateResponse.setTakeDown(false);
    itemsPriceStockImagesUpdateResponse.setEditedResizeAndImagesUpdateStatusResponse(
      new EditedResizeAndImagesUpdateStatusResponse());
    Mockito.when(productLevel3V2Service.toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
      Mockito.any(EditProductResponse.class))).thenReturn(new ProductVariantUpdateRequest());
    Mockito.when(
        productLevel3V2Service.editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
          Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class)))
      .thenReturn(itemsPriceStockImagesUpdateResponse);
    Mockito.when(
        productLevel3V2Service.updateLogistics(Mockito.any(ProductLevel3UpdateRequest.class), Mockito.anyBoolean(),
          Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean()))
      .thenReturn(ApiErrorCode.INVALID_DATA_INPUT);
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(),Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3V2Service)
      .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service)
      .updateEditInfo(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
        Mockito.any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
      .editPriceStockVariantsInfo(Mockito.anyString(), Mockito.any(ProductLevel3.class),
        Mockito.any(ProductVariantUpdateRequest.class), Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(Mockito.any(ProductL3UpdateRequest.class),
      Mockito.any(EditProductResponse.class));
    Mockito.verify(productLevel3V2Service)
      .updateLogistics(Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
        Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).takeDownProduct(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void validationForEditRequestValidationForL5SucessFalseTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    editProductResponse.setApiErrorCode(null);
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse1.setApiErrorCode(null);
    editProductResponse.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    editProductResponse.setProductReview(true);
    editProductResponse1.setToTakeDown(true);
    productEditValidationDTO1.setEditProductResponse(editProductResponse);
    productEditValidationDTO1.setProductL3UpdateRequest(new ProductL3UpdateRequest());
    productEditValidationDTO1.setProductL3Response(new ProductL3Response());
    productEditValidationDTO1.setProductLevel3(new ProductLevel3());
    productEditValidationDTO.setL5ValidationFailed(true);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
  }


  @Test
  public void validationForEditRequestProductEditValidationForNeedCorrectionTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse.setApiErrorCode(null);
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setNeedCorrection(true);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
            productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
                Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
              Mockito.isNull(),Mockito.any()))
        .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), any(), anyBoolean(),
        any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO);
    when(productLevel3V2Service.editPriceStockVariantsInfo(any(), Mockito.any(),
        any(), any())).thenReturn(new ItemsPriceStockImagesUpdateResponse());
    when(productLevel3V2Service.generateProductLevel3(any())).thenReturn(new ProductLevel3());
    when(productLevel3V2Service.updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(), any(),
        any(), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(new EditProductResponse());
    productEditValidationDTO =
        productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
            false);
    Mockito.verify(productLevel3V2Service)
        .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
          Mockito.isNull(), Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productLevel3V2Service)
        .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(),
            Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).editPriceStockVariantsInfo(any(), Mockito.any(),
        any(), any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(any());
    Mockito.verify(productLevel3V2Service).updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(), any(),
        any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
        .updateLogistics(any(), anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean());
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(any(), any());
  }

  @Test
  public void validationForEditRequestProductEditValidationForNeedCorrectionAutoRejectTest() throws Exception {
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse.setApiErrorCode(null);
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setNeedCorrection(true);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(),Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), any(), anyBoolean(),
      any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(productEditValidationDTO);
    when(productLevel3V2Service.editPriceStockVariantsInfo(any(), Mockito.any(),
      any(), any())).thenReturn(new ItemsPriceStockImagesUpdateResponse());
    when(productLevel3V2Service.generateProductLevel3(any())).thenReturn(new ProductLevel3());
    when(productLevel3V2Service.updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(),
      any(), any(), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(new EditProductResponse());
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(),Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productLevel3V2Service)
      .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).editPriceStockVariantsInfo(any(), Mockito.any(),
      any(), any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(any());
    Mockito.verify(productLevel3V2Service).updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(), any(),
      any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
      .updateLogistics(any(), anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean());
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(any(), any());
  }

  @Test
  public void validationForEditRequestProductEditValidationForNeedCorrectionTestWithCombinedEditEnabled() throws Exception {
    ReflectionTestUtils.setField(productLevel3V2ServiceWrapperBean, "combinedEditFlowEnabled", true);
    productL3UpdateRequest.setNeedCorrection(true);
    Mockito.when(productLevel3V2Service.generateProductLevel3(productL3UpdateRequest)).thenReturn(productLevel3);
    ProductEditValidationDTO productEditValidationDTO1 = new ProductEditValidationDTO();
    EditProductResponse editProductResponse1 = new EditProductResponse();
    editProductResponse.setApiErrorCode(null);
    editProductResponse1.setApiErrorCode(null);
    productEditValidationDTO1.setEditProductResponse(editProductResponse1);
    productEditValidationDTO1.setNeedCorrection(true);
    productEditValidationDTO.setL5ValidationFailed(false);
    Mockito.when(
        productLevel3V2Service.validationsForEdit(Mockito.anyString(), Mockito.any(ProductL3UpdateRequest.class),
          Mockito.any(ProductLevel3.class), Mockito.any(EditProductResponse.class),
          Mockito.isNull(),Mockito.any()))
      .thenReturn(productEditValidationDTO);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
      .thenReturn(profileResponse);
    when(productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(),
      Mockito.any(), any(), anyBoolean(),any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(
      productEditValidationDTO);
    when(productLevel3V2Service.editPriceStockVariantsInfo(any(), Mockito.any(),
      any(), any())).thenReturn(new ItemsPriceStockImagesUpdateResponse());
    when(productLevel3V2Service.generateProductLevel3(any())).thenReturn(new ProductLevel3());
    when(productLevel3V2Service.updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(), any(),
      any(), anyBoolean(), eq(null), eq(productL3UpdateRequest))).thenReturn(new EditProductResponse());
    productEditValidationDTO =
      productLevel3V2ServiceWrapperBean.editProductDetails(REQUEST_ID, productL3UpdateRequest, true, false,
        false);
    Mockito.verify(productLevel3V2Service)
      .validationsForEdit(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.isNull(),Mockito.any());
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(productLevel3V2Service)
      .validationForProductL3ResponseAndNeedRevisionUpdate(Mockito.any(), Mockito.any(), Mockito.any(),
        Mockito.anyBoolean(), any(), Mockito.anyList(), Mockito.any(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(Mockito.any());
    Mockito.verify(productLevel3V2Service).generateProductLevel3(any());
    Mockito.verify(productLevel3V2Service).updateEditInfo(any(), anyBoolean(), anyBoolean(), anyBoolean(), any(),
      any(), anyBoolean(), eq(null), eq(productL3UpdateRequest));
    Mockito.verify(productLevel3V2Service)
      .updateLogistics(any(), anyBoolean(), anyBoolean(), anyBoolean(), anyBoolean());
    Mockito.verify(productLevel3V2Service).toProductVariantUpdateRequest(any(), any());
    Mockito.verify(productLevel3V2Service).editPriceStockVariantsInfo(any(), Mockito.any(),
      any(), any());
  }

  @Test
  public void updateOff2OnChannelActiveInRequestTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(true);
    profileResponse.setCompany(companyDTO);
    productL3UpdateRequest.setOff2OnChannelActive(true);
    productLevel3V2ServiceWrapperBean.updateOff2OnChannelActiveInRequest(productL3UpdateRequest, profileResponse);
    Assertions.assertTrue(productL3UpdateRequest.isOff2OnChannelActive());
    profileResponse.getCompany().setOfflineToOnlineFlag(false);
    productLevel3V2ServiceWrapperBean.updateOff2OnChannelActiveInRequest(productL3UpdateRequest, profileResponse);
    Assertions.assertFalse(productL3UpdateRequest.isOff2OnChannelActive());
  }

  @Test
  public void setDefaultB2CActivatedTest() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(false);
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    productLevel3V2ServiceWrapperBean.setDefaultB2CActivated(profileResponse,
        productL3UpdateRequest);
    Assertions.assertTrue(productL3UpdateRequest.getB2cActivated());
  }

  @Test
  public void setDefaultB2CActivatedTest_B2CAndB2BActivated() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(false);
    companyDTO.setSalesChannel(
        Arrays.asList(Constants.B2C_SELLER_CHANNEL, Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    productLevel3V2ServiceWrapperBean.setDefaultB2CActivated(profileResponse,
        productL3UpdateRequest);
    Assertions.assertFalse(productL3UpdateRequest.getB2cActivated());
  }

  @Test
  public void setDefaultB2CActivatedTest_OnlyB2BActivated() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(false);
    companyDTO.setSalesChannel(
        Arrays.asList(Constants.B2B_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    productLevel3V2ServiceWrapperBean.setDefaultB2CActivated(profileResponse,
        productL3UpdateRequest);
    Assertions.assertFalse(productL3UpdateRequest.getB2cActivated());
  }

  @Test
  public void setDefaultB2CActivatedTest_B2CActivatedAndInstoreSeller() {
    ProductL3UpdateRequest productL3UpdateRequest = new ProductL3UpdateRequest();
    productL3UpdateRequest.setB2cActivated(false);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(true);
    companyDTO.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    profileResponse.setCompany(companyDTO);
    productLevel3V2ServiceWrapperBean.setDefaultB2CActivated(profileResponse,
        productL3UpdateRequest);
    Assertions.assertFalse(productL3UpdateRequest.getB2cActivated());
  }

  @Test()
  public void processUpdateForEditFlowTest() throws Exception {
    productEditValidationDTO.setNeedCorrection(true);
    EditProductResponse response =
        this.productLevel3V2ServiceWrapperBean.processUpdateForEditFlow(false, false, false, editProductResponse,
            productEditValidationDTO, new ProfileResponse(), false, null);
    Assertions.assertEquals(response, editProductResponse);
  }

  @Test()
  public void getAttributeCodeAndIdMapNullTest() throws Exception {
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeCodeAndIdMap(null);
    Assertions.assertEquals(0, response.size());
  }

  @Test()
  public void getAttributeCodeAndIdMapTest() throws Exception {
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeCodeAndIdMap(productL3Response);
    Assertions.assertEquals(1, response.size());
  }

  @Test()
  public void getAttributeCodeAndIdMapMasterDataAttributeNullTest() throws Exception {
    productL3Response.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataAttribute(null);
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeCodeAndIdMap(productL3Response);
    Assertions.assertEquals(0, response.size());
  }

  @Test()
  public void getAttributeCodeAndIdMapMasterDataProductNullTest() throws Exception {
    productL3Response.setMasterDataProduct(null);
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeCodeAndIdMap(productL3Response);
    Assertions.assertEquals(0, response.size());
  }

  @Test()
  public void getAttributeCodeAndIdMapMasterDataProductNonNullTest() throws Exception {
    productL3Response.setMasterDataProduct(new MasterDataProductDTO());
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeCodeAndIdMap(productL3Response);
    Assertions.assertEquals(0, response.size());
  }

  @Test()
  public void getAttributeIdAndValueTest() throws Exception {
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(FAMILY_COLOUR_ATTRIBUTE_CODE, ATTRIBUTE_VALUE_2);
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeIdAndValue(new TreeMap<>(),
        productVariantPriceStockAndImagesRequest);
    Assertions.assertEquals(0, response.size());
  }

  @Test()
   void getAttributeIdAndValue_Test() throws Exception {
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_2);
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(ATTRIBUTE_CODE, ATTRIBUTE_VALUE_1);
    productVariantPriceStockAndImagesRequest.getAttributesMap().put(ATTRIBUTE_ID, ATTRIBUTE_VALUE_1);
    Map<String, String> response = this.productLevel3V2ServiceWrapperBean.getAttributeIdAndValue(new TreeMap<>(),
        productVariantPriceStockAndImagesRequest);
    Assertions.assertEquals(0, response.size());
  }

  @Test
  void editProductMasterData_validInput_returnsNull() throws Exception {
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertNull(result);
  }


  @Test
  void editProductMasterData_validInput_returnsDescriptionApiErrorCode() throws Exception {
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    Mockito.when(
        productLevel3V2Service.validateExistingOrderOnProductNameEdit(productMasterDataEditRequest))
      .thenReturn(ApiErrorCode.PRODUCT_HAS_ORDER);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertNotNull(result);
  }

  @Test
   void editProductMasterData_blankStoreId_throwsException() {
    Exception ex = Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> productLevel3V2ServiceWrapperBean.editProductMasterData(StringUtils.EMPTY, REQUEST_ID,
        USER_NAME, productMasterDataEditRequest));
    Assertions.assertTrue(ex.getMessage().contains(ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK));
  }

  @Test
   void editProductMasterData_blankProductSku_throwsException() {
    productMasterDataEditRequest.setProductSku(StringUtils.EMPTY);
    Exception ex = Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest));
    Assertions.assertTrue(ex.getMessage().contains(ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY));
  }

  @Test
   void editProductMasterData_blankProductCode_throwsException() {
    productMasterDataEditRequest.setProductCode(null);
    Exception ex = Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest));
    Assertions.assertTrue(ex.getMessage().contains(ErrorMessages.PRODUCT_CODE_BLANK));
  }

  @Test
   void editProductMasterData_invalidDescription_throwsException() throws Exception {
    productMasterDataEditRequest.setDescription(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
          productMasterDataEditRequest));
  }

  @Test
   void editProductMasterData_EmptyDescriptionInstoreSeller() throws Exception {
    productMasterDataEditRequest.setDescription(null);
    productMasterDataEditRequest.setInstore(true);
    productMasterDataEditRequest.setB2cActivated(false);
    productMasterDataEditRequest.setSizeChartCode(INVALID_SIZE_CHART_CODE);
    ApiErrorCode apiErrorCode =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertNull(apiErrorCode);
  }

  @Test
   void editProductMasterData_descriptionTooLong_throwsException() throws Exception {
    productMasterDataEditRequest.setProductName(LONG_PRODUCT_NAME);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
          productMasterDataEditRequest));
  }

  @Test
   void editProductMasterData_invalidYoutubeUrl_throwsException() throws Exception {
    productMasterDataEditRequest.setUrl(USER_NAME);
    Mockito.doNothing().when(productLevel3V2Service).validateDescriptionLength(Mockito.any());
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION))
      .when(productLevel3V2Service).validateYoutubeUrlIsValid(Mockito.any(), Mockito.isNull());
    try {
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertNotNull(e);
    } finally {
      Mockito.verify(productLevel3V2Service)
        .validateDescriptionLength(productMasterDataEditRequest.getDescription());
      Mockito.verify(productLevel3V2Service)
        .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
      Mockito.verify(productLevel3V2Service)
        .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    }
  }

  @Test
   void editProductMasterData_invalidSizeChartCode_throwsException() throws Exception {
    productMasterDataEditRequest.setSizeChartCode(INVALID_SIZE_CHART_CODE);
    productMasterDataEditRequest.setMasterDataEditChangeTypes(
      Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE));
    Mockito.doNothing().when(productLevel3V2Service).validateDescriptionLength(Mockito.any());
    Mockito.doNothing().when(productLevel3V2Service)
      .validateYoutubeUrlIsValid(Mockito.any(), Mockito.isNull());
    Mockito.when(
        productService.validateSizeChart(Mockito.eq(INVALID_SIZE_CHART_CODE), Mockito.any()))
      .thenReturn(ApiErrorCode.SIZE_CHART_CODE_INVALID);
    ApiErrorCode apiErrorCode =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertNotNull(apiErrorCode);
  }

  @Test
  void editProductMasterData_SizeChartCodeChange_noError() throws Exception {
    productMasterDataEditRequest.setSizeChartCode(INVALID_SIZE_CHART_CODE);
    productMasterDataEditRequest.setMasterDataEditChangeTypes(
      Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE));
    Mockito.doNothing().when(productLevel3V2Service).validateDescriptionLength(Mockito.any());
    Mockito.doNothing().when(productLevel3V2Service)
      .validateYoutubeUrlIsValid(Mockito.any(), Mockito.isNull());
    Mockito.when(
        productService.validateSizeChart(Mockito.eq(INVALID_SIZE_CHART_CODE), Mockito.any()))
      .thenReturn(null);
    ApiErrorCode apiErrorCode =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Assertions.assertNull(apiErrorCode);
  }


  @Test
  void editProductMasterData_validInput_returnsWithSizeChartRemoved() throws Exception {
    productMasterDataEditRequest.setSizeChartCode(StringUtils.EMPTY);
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE));
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertNull(result);
  }

  @Test
  void editProductMasterData_validInput_returnsWithShiippingError() throws Exception {
    productMasterDataEditRequest.setSizeChartCode(StringUtils.EMPTY);
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE));
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(ApiErrorCode.SHIPPING_WEIGHT_NOT_VALID);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Assertions.assertEquals(ApiErrorCode.SHIPPING_WEIGHT_NOT_VALID, result);
  }

  @Test
   void editProductMasterData_shippingDimensionValidationFails_returnsErrorCode() throws Exception {
    productMasterDataEditRequest.setShippingWeight(0.0);
    Mockito.doNothing().when(productLevel3V2Service)
        .validateDescriptionLength(Mockito.any());
    Mockito.doNothing().when(productLevel3V2Service)
        .validateYoutubeUrlIsValid(Mockito.any(), Mockito.isNull());
    Mockito.when(productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false)).thenReturn(ApiErrorCode.SHIPPING_WEIGHT_NOT_VALID);
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
      productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Assertions.assertEquals(ApiErrorCode.DIMENSION_LESS_THAN_ZERO, result);
  }

  @Test
  void editProductMasterData_validInputForProductTypeUpdateToBOPIS() throws Exception {
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);
    productMasterDataEditRequest.setSellerBopisFlag(true);
    SimpleBooleanResponse cncResponse = new SimpleBooleanResponse();
    cncResponse.setResult(true);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    Mockito.when(xProductOutbound.getCncAtL5ByProductSku(PRODUCT_SKU)).thenReturn(cncResponse);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Mockito.verify(xProductOutbound).getCncAtL5ByProductSku(PRODUCT_SKU);
    Assertions.assertNotNull(result);
  }

  @Test
  void editProductMasterData_validInputForProductTypeUpdateToBOPIS_sellerNotEligible() throws Exception {
    SimpleBooleanResponse cncResponse = new SimpleBooleanResponse();
    cncResponse.setResult(true);
    productMasterDataEditRequest.setProductType(ProductType.BOPIS.getProductType());
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);
    productMasterDataEditRequest.setSellerBopisFlag(false);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    Mockito.when(xProductOutbound.getCncAtL5ByProductSku(PRODUCT_SKU)).thenReturn(cncResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productOutbound).getCategoryDetailByCategoryCode(CATEGORY_CODE);
    Assertions.assertNotNull(result);
    Mockito.verify(xProductOutbound).getCncAtL5ByProductSku(PRODUCT_SKU);
  }

  @Test
  void editProductMasterData_validInputForProductTypeUpdateToRegular_sellerNotEligible() throws Exception {
    SimpleBooleanResponse cncResponse = new SimpleBooleanResponse();
    cncResponse.setResult(true);
    productMasterDataEditRequest.setProductType(ProductType.REGULAR.getProductType());
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);
    productMasterDataEditRequest.setSellerBopisFlag(false);
    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setBopisEligible(true);
    Mockito.when(xProductOutbound.getCncAtL5ByProductSku(PRODUCT_SKU)).thenReturn(cncResponse);
    Mockito.when(productOutbound.getCategoryDetailByCategoryCode(CATEGORY_CODE)).thenReturn(categoryDetailResponse);
    Mockito.when(
        productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false))
      .thenReturn(null);
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(STORE_ID, REQUEST_ID, USER_NAME,
        productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(xProductOutbound).getCncAtL5ByProductSku(PRODUCT_SKU);
    Assertions.assertNull(result);
  }

  @Test
  void performRestrictedKeywordChecks_noRelevantChangeTypes_doesNothing() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Collections.emptySet());

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verifyNoInteractions(productLevel3Helper);
    Mockito.verifyNoInteractions(productService);
    Mockito.verifyNoInteractions(productRepository);
    Mockito.verifyNoInteractions(productOutbound);
  }

  @Test
  void performRestrictedKeywordChecks_restrictedKeywordsFound_autoRejectAction() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("badword"))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Mockito.verify(productService).checkActiveOrderDataToSkipRestrictedKeywordAction(productMasterDataEditRequestArgumentCaptor.capture(), eq(restrictedInfo));
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertFalse(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty());
  }

  @Test
  void performRestrictedKeywordChecks_restrictedKeywordsFound_autoNeedRevisionAction() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE));

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("DESCRIPTION", List.of("anotherbadword"))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertFalse(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty());
  }

  @Test
  void performRestrictedKeywordChecks_restrictedKeywordsFound_manualReviewDefaultAction() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("offensive"))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertFalse(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty());
  }

  @Test
  void performRestrictedKeywordChecks_restrictedKeywordsFound_changeCategoryAndAutoApproveAction() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("forbidden"))));
    restrictedInfo.setCategoryRestrictedKeywordId("restrictedKeywordId");
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    CategoryRestrictedKeywordResponse categoryKeywordResponse = new CategoryRestrictedKeywordResponse();
    categoryKeywordResponse.setDestinationCategory(NEW_CATEGORY_CODE);
    when(productRepository.getCategoryRestrictedKeywordDetail("restrictedKeywordId")).thenReturn(categoryKeywordResponse);

    CategoryResponse newCategory = new CategoryResponse();
    newCategory.setCategoryCode(NEW_CATEGORY_CODE);
    newCategory.setName(NEW_CATEGORY_NAME);
    when(productOutbound.getCategoryBasicDetailByCategoryCode(NEW_CATEGORY_CODE)).thenReturn(newCategory);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Mockito.verify(productRepository).getCategoryRestrictedKeywordDetail("restrictedKeywordId");
    Mockito.verify(productOutbound).getCategoryBasicDetailByCategoryCode(NEW_CATEGORY_CODE);

    Assertions.assertEquals(NEW_CATEGORY_CODE, productMasterDataEditRequest.getCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, productMasterDataEditRequest.getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, productCollection.getCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, productCollection.getCategoryName());
    Assertions.assertFalse(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertTrue(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty()); // Should be empty after reset
  }

  @Test
  void performRestrictedKeywordChecks_restrictedKeywordsFound_SkipAllAction() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(Constants.SKIP_ALL_ACTIONS);
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("forbidden"))));
    restrictedInfo.setCategoryRestrictedKeywordId("restrictedKeywordId");
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    CategoryRestrictedKeywordResponse categoryKeywordResponse = new CategoryRestrictedKeywordResponse();
    categoryKeywordResponse.setDestinationCategory(NEW_CATEGORY_CODE);
    when(productRepository.getCategoryRestrictedKeywordDetail("restrictedKeywordId")).thenReturn(categoryKeywordResponse);

    CategoryResponse newCategory = new CategoryResponse();
    newCategory.setCategoryCode(NEW_CATEGORY_CODE);
    newCategory.setName(NEW_CATEGORY_NAME);
    when(productOutbound.getCategoryBasicDetailByCategoryCode(NEW_CATEGORY_CODE)).thenReturn(newCategory);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));

    Assertions.assertEquals(CATEGORY_CODE, productMasterDataEditRequest.getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE, productMasterDataEditRequest.getCategoryName());
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertTrue(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty()); // Should be empty after reset
  }


  @Test
  void performRestrictedKeywordChecks_changeCategoryAndAutoApproveAction_sameCategoryCode() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));
    productMasterDataEditRequest.setCategoryCode(CATEGORY_CODE);

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("forbidden"))));
    restrictedInfo.setCategoryRestrictedKeywordId("restrictedKeywordId");
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    CategoryRestrictedKeywordResponse categoryKeywordResponse = new CategoryRestrictedKeywordResponse();
    categoryKeywordResponse.setDestinationCategory(CATEGORY_CODE); // Same category
    when(productRepository.getCategoryRestrictedKeywordDetail("restrictedKeywordId")).thenReturn(categoryKeywordResponse);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Mockito.verify(productRepository).getCategoryRestrictedKeywordDetail("restrictedKeywordId");
    Mockito.verifyNoInteractions(productOutbound); // Should not call getCategoryBasicDetailByCategoryCode

    Assertions.assertEquals(CATEGORY_CODE, productMasterDataEditRequest.getCategoryCode()); // Category code should remain the same
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertFalse(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty()); // Should still have restricted keywords if category not changed
  }

  @Test
  void performRestrictedKeywordChecks_noRestrictedKeywordsFound() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(Collections.emptyList()); // No restricted keywords
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);


    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Assertions.assertFalse(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertTrue(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty());
  }

  @Test
  void performRestrictedKeywordChecks_isDefinitiveActionToBeSkipped_true() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));

    ProductCollection productCollection = new ProductCollection();
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
      .thenReturn(productCollection);

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType()); // Any action
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField("PRODUCT_NAME", List.of("badword"))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    productLevel3V2ServiceWrapperBean.performRestrictedKeywordChecks(STORE_ID, productMasterDataEditRequest, masterProductEditDTO);

    Mockito.verify(productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productLevel3Helper).getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE));
    Mockito.verify(productService).checkActiveOrderDataToSkipRestrictedKeywordAction(productMasterDataEditRequestArgumentCaptor.capture(), eq(restrictedInfo));
    Assertions.assertTrue(masterProductEditDTO.getProductCollection().isRestrictedKeywordsPresent());
    Assertions.assertFalse(masterProductEditDTO.getRestrictedKeywordsByFieldList().isEmpty());
  }

  @Test
  void editProductMasterData_withContentChange_setsPostLiveReviewConfig() throws Exception {
    // Setup product master data request with content changes
    productMasterDataEditRequest.setMasterDataEditChangeTypes(
        Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE));

    // Mock product collection repository response
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);

    // Mock review configuration response
    ConfigurationStatusRequest expectedRequest = ConfigurationStatusRequest.builder()
        .categoryCode(CATEGORY_CODE)
        .businessPartnerCode(productMasterDataEditRequest.getBusinessPartnerCode())
        .build();

    ConfigurationStatusResponse configResponse = new ConfigurationStatusResponse();
    configResponse.setReviewConfig(Constants.POST_LIVE);
    when(productOutbound.getReviewConfiguration(Collections.singletonList(expectedRequest)))
        .thenReturn(Collections.singletonList(configResponse));

    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField(
      PRODUCT_NAME, List.of(RESTRICTED_KEYWORD))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(new RestrictedKeywordsByFieldAndActionType(Collections.emptyList()));
    ApiErrorCode result =
      productLevel3V2ServiceWrapperBean.editProductMasterData(
        STORE_ID, REQUEST_ID, USER_NAME, productMasterDataEditRequest);

    // Verify
    Mockito.verify(productOutbound).getReviewConfiguration(Collections.singletonList(expectedRequest));
    Mockito.verify(productLevel3V2Service).validateDescriptionLength(any());
    Mockito.verify(productLevel3V2Service).validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Assertions.assertTrue(productCollection.isPostLive());
    Assertions.assertNull(result);
  }

  @Test
  void editProductMasterData_withoutContentChange_doesNotSetReviewConfig() throws Exception {
    productMasterDataEditRequest.setMasterDataEditChangeTypes(
        Set.of(L3InfoUpdateChangeType.SIZE_CHART_UPDATE, L3InfoUpdateChangeType.DESCRIPTION_UPDATE));
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    ConfigurationStatusRequest expectedRequest = ConfigurationStatusRequest.builder()
      .categoryCode(CATEGORY_CODE)
      .businessPartnerCode(productMasterDataEditRequest.getBusinessPartnerCode())
      .build();
    ConfigurationStatusResponse configResponse = new ConfigurationStatusResponse();
    configResponse.setReviewConfig(Constants.PRE_LIVE);
    when(productOutbound.getReviewConfiguration(Collections.singletonList(expectedRequest)))
      .thenReturn(Collections.singletonList(configResponse));
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);
    RestrictedKeywordsByFieldAndActionType restrictedInfo = new RestrictedKeywordsByFieldAndActionType();
    restrictedInfo.setAction(RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType());
    restrictedInfo.setRestrictedKeywordsByFieldList(List.of(new RestrictedKeywordsByField(
      PRODUCT_NAME, List.of(RESTRICTED_KEYWORD))));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(any(ProductDetailResponse.class), eq(CATEGORY_CODE)))
      .thenReturn(restrictedInfo);
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(
        STORE_ID, REQUEST_ID, USER_NAME, productMasterDataEditRequest);
    Mockito.verify(productOutbound).getReviewConfiguration(any());
    Mockito.verify(productLevel3V2Service).validateDescriptionLength(any());
    Mockito.verify(productLevel3V2Service).validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Assertions.assertFalse(productCollection.isPostLive());
    Assertions.assertNull(result);
  }

  @Test
  void editProductMasterData_withContentChangeButNotPostLiveConfig_setsNotPostLive() throws Exception {
    // Setup product master data request with content changes
    productMasterDataEditRequest.setMasterDataEditChangeTypes(
        Set.of(L3InfoUpdateChangeType.DESCRIPTION_UPDATE));

    // Mock product collection repository response
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    when(productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productCollection);

    // Mock review configuration response with non-post-live config
    ConfigurationStatusRequest expectedRequest = ConfigurationStatusRequest.builder()
        .categoryCode(CATEGORY_CODE)
        .businessPartnerCode(productMasterDataEditRequest.getBusinessPartnerCode())
        .build();

    ConfigurationStatusResponse configResponse = new ConfigurationStatusResponse();
    configResponse.setReviewConfig(Constants.PRE_LIVE);
    when(productOutbound.getReviewConfiguration(Collections.singletonList(expectedRequest)))
        .thenReturn(Collections.singletonList(configResponse));
    when(productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(
      any(ProductDetailResponse.class), eq(CATEGORY_CODE))).thenReturn(
      new RestrictedKeywordsByFieldAndActionType(Collections.emptyList()));
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(
        STORE_ID, REQUEST_ID, USER_NAME, productMasterDataEditRequest);

    Mockito.verify(productOutbound).getReviewConfiguration(Collections.singletonList(expectedRequest));
    Mockito.verify(productLevel3V2Service).validateDescriptionLength(any());
    Mockito.verify(productLevel3V2Service).validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productOutbound).getReviewConfiguration(Collections.singletonList(expectedRequest));
    Assertions.assertFalse(productCollection.isPostLive());
    Assertions.assertNull(result);
  }

  @Test
  void editProductMasterData_NewImagesAdded_ShouldPublishResizeEvent() throws Exception {
    // Given
    String storeId = "10001";
    String requestId = "requestId";
    String username = "username";

    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageRequest.setReviewType("NEW");
    imageRequest.setMainImage(true);
    imageRequest.setLocationPath(LONG_PRODUCT_NAME);
    imageRequests.add(imageRequest);
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setMaxProductDimensionLimit(100);

    // When
    when(productService.performMasterDataUpdate(any(), any(), any(), any(), any())).thenReturn(ApiErrorCode.SUCCESS_VALIDATION);
    when(fileStorageService.generateFinalImageFullPath(any())).thenReturn(LONG_PRODUCT_NAME.concat(FINAL_PATH));
    // Then
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(storeId,
      requestId, username, productMasterDataEditRequest);
    Assertions.assertEquals(ApiErrorCode.SUCCESS_VALIDATION, result);
    Mockito.verify(productPublisherService).publishEditImageResizeEvent(any(EditedImageResizeEvent.class));
    Mockito.verify(productLevel3V2Service).validateDescriptionLength(any());
    Mockito.verify(productLevel3V2Service).validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
  }

  @Test
  void editProductMasterData_NewImagesAdded_ShouldPublishResizeEventForEdited() throws Exception {
    // Given
    String storeId = "10001";
    String requestId = "requestId";
    String username = "username";

    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageRequest.setReviewType("UPDATE");
    imageRequest.setMainImage(true);
    imageRequest.setLocationPath(LONG_PRODUCT_NAME);
    imageRequests.add(imageRequest);
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setMaxProductDimensionLimit(100);

    // When
    when(productService.performMasterDataUpdate(any(), any(), any(), any(), any())).thenReturn(ApiErrorCode.SUCCESS_VALIDATION);
    when(fileStorageService.generateFinalImageFullPath(any())).thenReturn(LONG_PRODUCT_NAME.concat(FINAL_PATH));
    // Then
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(storeId,
      requestId, username, productMasterDataEditRequest);
    Assertions.assertEquals(ApiErrorCode.SUCCESS_VALIDATION, result);
    Mockito.verify(productLevel3V2Service).validateDescriptionLength(any());
    Mockito.verify(productLevel3V2Service).validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    Mockito.verify(productLevel3V2Service).validateYoutubeUrlIsValid(productMasterDataEditRequest
      , null);
    Mockito.verify(productLevel3V2Service).validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
  }

  @Test
   void editProductMasterData_NoContentChangeNoNewImages_ShouldNotPublishEvents() throws Exception {
    // Given
    String storeId = "10001";
    String requestId = "requestId";
    String username = "username";

    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setMaxProductDimensionLimit(100);
    masterProductEditDTO.setPublishImageQcForContentChange(false);

    // When
    when(productService.performMasterDataUpdate(any(), any(), any(), any(), any())).thenReturn(ApiErrorCode.SUCCESS_VALIDATION);

    // Then
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(storeId,
      requestId, username, productMasterDataEditRequest);
    Assertions.assertEquals(ApiErrorCode.SUCCESS_VALIDATION, result);
    Mockito.verify(productPublisherService, Mockito.never()).publishEditImageResizeEvent(any());
    Mockito.verify(productService, Mockito.never()).publishImageQcEventForContentEdit(any(), any(),
      any(),
      anyBoolean());
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
  }

  @Test
   void editProductMasterData_ContentChangedWithNewImages_ShouldOnlyPublishResizeEvent() throws Exception {
    // Given
    String storeId = "10001";
    String requestId = "requestId";
    String username = "username";

    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageRequest.setReviewType("NEW");
    imageRequest.setMainImage(true);
    imageRequest.setLocationPath(LONG_PRODUCT_NAME);
    imageRequests.add(imageRequest);
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setMaxProductDimensionLimit(100);
    masterProductEditDTO.setPublishImageQcForContentChange(true);
    when(fileStorageService.generateFinalImageFullPath(any())).thenReturn(LONG_PRODUCT_NAME.concat(FINAL_PATH));
    // When
    when(productService.performMasterDataUpdate(any(), any(), any(), any(), any())).thenReturn(ApiErrorCode.SUCCESS_VALIDATION);

    // Then
    ApiErrorCode result = productLevel3V2ServiceWrapperBean.editProductMasterData(storeId,
      requestId, username, productMasterDataEditRequest);
    Assertions.assertEquals(ApiErrorCode.SUCCESS_VALIDATION, result);
    Mockito.verify(productPublisherService).publishEditImageResizeEvent(any());
    Mockito.verify(productService, Mockito.never()).publishImageQcEventForContentEdit(any(), any(),
      any(),
      anyBoolean());
    Mockito.verify(productLevel3V2Service)
      .validateDescriptionLength(productMasterDataEditRequest.getDescription());
    Mockito.verify(productLevel3V2Service)
      .validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    Mockito.verify(productLevel3V2Service)
      .validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    Mockito.verify(productLevel3V2Service)
      .validateExistingOrderOnProductNameEdit(productMasterDataEditRequest);
    verify(productLevel3Service).takeDownOrReactivateProduct(storeId,
      productMasterDataEditRequest.getProductSku(), true, null,
      null);
  }

  @Test
   void publishImageQCForContentChange_WhenContentChangedWithExistingCategories_ShouldUseExistingCategories() {
    // Given
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductCode("productCode");
    request.setProductLevel3SummaryDetailsImageRequests(new ArrayList<>());

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setPublishImageQcForContentChange(true);

    List<CategoryResponse> existingCategories = new ArrayList<>();
    CategoryResponse category = new CategoryResponse();
    category.setCategoryCode("existingCategoryCode");
    category.setName("existingCategoryName");
    existingCategories.add(category);
    masterProductEditDTO.setCategoryResponses(existingCategories);

    // When
    productLevel3V2ServiceWrapperBean.publishImageQCForContentChange(request, masterProductEditDTO);

    // Then
    Mockito.verify(productService).publishImageQcEventForContentEdit(
      anyList(),
      any(),
      any(),
      eq(false)
    );
  }

  @Test
   void publishImageQCForContentChange_WhenContentChangedWithExistingCategories_Categories() {
    // Given
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductCode("productCode");
    request.setProductLevel3SummaryDetailsImageRequests(new ArrayList<>());

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setPublishImageQcForContentChange(true);

    List<CategoryResponse> existingCategories = new ArrayList<>();
    CategoryResponse category = new CategoryResponse();
    category.setCategoryCode("existingCategoryCode");
    category.setName("existingCategoryName");
    existingCategories.add(category);
    // When
    productLevel3V2ServiceWrapperBean.publishImageQCForContentChange(request, masterProductEditDTO);

    // Then
    Mockito.verify(productService).publishImageQcEventForContentEdit(
      anyList(),
      any(),
      any(),
      eq(false)
    );
  }

  @Test
   void publishImageQCForContentChange_WhenHasNewImages_ShouldNotPublishQCEvent() {
    // Given
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    List<ProductLevel3SummaryDetailsImageRequest> imageRequests = new ArrayList<>();
    ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
    imageRequest.setReviewType("NEW");
    imageRequests.add(imageRequest);
    request.setProductLevel3SummaryDetailsImageRequests(imageRequests);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setPublishImageQcForContentChange(true);

    // When
    productLevel3V2ServiceWrapperBean.publishImageQCForContentChange(request, masterProductEditDTO);

    // Then
    Assertions.assertTrue(request.isPublishImageQcForContentChange());
    Mockito.verify(productService, Mockito.never()).publishImageQcEventForContentEdit(any(), any(),
      any(),
      anyBoolean());
  }

  @Test
   void publishImageQCForContentChange_WhenFlagIsFalse_ShouldNotPublishQCEvent() {
    // Given
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductLevel3SummaryDetailsImageRequests(new ArrayList<>());

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setPublishImageQcForContentChange(false);

    // When
    productLevel3V2ServiceWrapperBean.publishImageQCForContentChange(request, masterProductEditDTO);

    // Then
    Assertions.assertFalse(request.isPublishImageQcForContentChange());
    Mockito.verify(productService, Mockito.never()).publishImageQcEventForContentEdit(any(), any(),
      any(), anyBoolean());
  }

  @Test
  void updateCategoryForAutoCategoryChange_WhenAutoCategoryChangeIsTrue_ShouldUpdateCategory() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setAutoCategoryChange(true);
    masterProductEditDTO.setCategoryCode(CATEGORY_CODE);

    // Act
    productLevel3V2ServiceWrapperBean.updateCategoryForAutoCategoryChange(request, masterProductEditDTO);

    // Assert
    Mockito.verify(productServiceWrapper).updateCategoryInPcb(
        eq(PRODUCT_CODE),
        eq(CATEGORY_CODE),
        eq(false),
        eq(BUSINESS_PARTNER_CODE)
    );
  }

  @Test
  void updateCategoryForAutoCategoryChange_WhenAutoCategoryChangeIsFalse_ShouldNotUpdateCategory() throws Exception {
    // Arrange
    ProductMasterDataEditRequest request = new ProductMasterDataEditRequest();
    request.setProductCode(PRODUCT_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);

    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    masterProductEditDTO.setAutoCategoryChange(false);
    masterProductEditDTO.setCategoryCode(CATEGORY_CODE);

    // Act
    productLevel3V2ServiceWrapperBean.updateCategoryForAutoCategoryChange(request, masterProductEditDTO);

    // Assert
    Mockito.verify(productServiceWrapper, Mockito.never()).updateCategoryInPcb(
        any(),
        any(),
        anyBoolean(),
        any()
    );
  }

  @Test
  public void checkIfOmniChannelSkuExistsResponseNullTest() {
    productLevel3V2ServiceWrapperBean.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Mockito.verify(productLevel3V2Service).checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Assertions.assertNotNull(STORE_ID);
  }

  @Test
  public void checkIfOmniChannelSkuExistsMapNullTest() {
    Mockito.when(productLevel3V2Service.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest()))
        .thenReturn(new ValidOmniChannelSkuResponse());
    OmniChannelMapAndSkuResponse validOmniChannelSkuResponse =
        productLevel3V2ServiceWrapperBean.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Mockito.verify(productLevel3V2Service).checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Assertions.assertNotNull(validOmniChannelSkuResponse);
  }

  @Test
  public void checkIfOmniChannelSkuExistsMapTest() {
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Map<String, ProductL1AndL2CodeResponse> map = new HashMap<>();
    map.put(PRODUCT_ID, new ProductL1AndL2CodeResponse());
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(map);
    Mockito.when(productLevel3V2Service.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest()))
        .thenReturn(validOmniChannelSkuResponse);
    productLevel3V2ServiceWrapperBean.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Mockito.verify(productLevel3V2Service).checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Assertions.assertNotNull(validOmniChannelSkuResponse);
  }

  @Test
  public void checkIfOmniChannelSkuExistsMap2Test() {
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse = new ValidOmniChannelSkuResponse();
    Map<String, ProductL1AndL2CodeResponse> map = new HashMap<>();
    ProductL1AndL2CodeResponse productL1AndL2CodeResponse = new ProductL1AndL2CodeResponse();
    productL1AndL2CodeResponse.setProductItemId(PRODUCT_ITEM_ID);
    map.put(PRODUCT_ID, productL1AndL2CodeResponse);
    validOmniChannelSkuResponse.setExistingOmniChannelSkusAndProductDetailsMap(map);
    Mockito.when(productLevel3V2Service.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest()))
        .thenReturn(validOmniChannelSkuResponse);
    Mockito.when(productItemBusinessPartnerService.findProductItemByProductItemIdIn(STORE_ID,
            Collections.singletonList(PRODUCT_ITEM_ID)))
        .thenReturn(Collections.singletonList(new ProductItemBusinessPartner()));
    productLevel3V2ServiceWrapperBean.checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Mockito.verify(productLevel3V2Service).checkIfOmniChannelSkuExists(STORE_ID, new OmniChannelSkuRequest());
    Mockito.verify(productItemBusinessPartnerService)
        .findProductItemByProductItemIdIn(STORE_ID, Collections.singletonList(PRODUCT_ITEM_ID));
    Assertions.assertNotNull(validOmniChannelSkuResponse);
  }

}
