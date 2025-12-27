package com.gdn.x.product.rest.web.controller.api;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.gdn.x.product.rest.web.model.request.PromoEligibilityRequest;
import com.gdn.x.product.rest.web.model.response.PromoEligibilityResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductsToItemCatalogMapping;
import com.gdn.x.product.model.vo.ReviewProductDetailVO;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.AgpConstant;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.CategoryBrandRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionProductActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemsRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategorySequenceRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.ProductsToItemCatalogMappingResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductCenterHistoryService;
import com.gdn.x.product.service.api.ProductDataArchivalService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductWrapperService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.common.web.param.MandatoryRequestParam;

public class ProductControllerTest {

  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private static final boolean INSTANT_PICKUP = false;

  private static final int FROM_DAY = 5;

  private static final int TO_DAY = 6;

  private static final int MONTH = 0;

  private static final int YEAR = 1;

  private static final int SIZE = 1;

  private static final int TOTAL = 1;

  private static final int PAGE = 0;

  private static final int SEQUENCE = 0;

  private static final String NEW_CATEGORY_CODE = "newCategoryCode";

  private static final String OLD_CATEGORY_CODE = "oldCategoryCode";

  private static final String CATALOG_CODE = "catalogCode";

  private static final String ITEM_SKU2 = "item-sku";

  private static final String SHOW_DELETED = "false";

  private static final String TRUE = "true";

  private static final AddProductAndItemsResponseVo ADD_PRODUCT_AND_ITEM_RESPONSE_VO =
      new AddProductAndItemsResponseVo();

  private static final String PRODUCT_CODE = "product-code";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ACTIVE = "Active";

  private static final String PRODUCT_CATENTRY_ID = "product-catentry-id";

  private static final boolean NEED_MASTER_DATA_DETAIL = true;

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String BLANK = "";

  private static final String ERROR_MESSAGE = "error message";

  private static final String PRODUCT_SKU = "product-sku";

  private static final String ITEM_SKU = ProductControllerTest.ITEM_SKU2;

  private static final String ITEM_CODE = "item-code";

  private static final String PRODUCT_SKU_INVALID = "invalid-product-sku";

  private static final String PRODUCT_CODE_INVALID = "invalid-product-sku";

  private static final boolean OVERWRITE_EXISTING_MASTER_DATA = true;

  private static final String BRAND = "brand";

  private static final String PRISTINE_ID = "PRI-000111-01";

  private static final String DEFAULT_SKU = "defaultSku";

  private static final String OFF_2_ON = "false";

  private static final Boolean SUSPENSION_TRUE = Boolean.TRUE;

  private static final Boolean SUSPENSION_FALSE = Boolean.FALSE;

  private static final String PREORDER_TYPE = "preOrderType";

  private static final int PREORDER_VALUE = 10;

  private static final String ACCESS_CHANNEL = "accessChannel";

  private static final Long PRODUCT_COUNT = 1000000000000L;
  private static final String unsyncProductError = "UNSYNCHRONIZE_PRODUCT_FAILED";

  @InjectMocks
  private ProductController productController;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ProductService productService;

  @Mock
  private ProductCenterHistoryService productCenterHistoryService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Mock
  private ProductCacheableService productCacheService;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private AsyncProcessor asyncProcessor;

  @Mock
  private ProductDataArchivalService productDataArchivalService;

  @Mock
  private ProductWrapperService productWrapperService;

  private MockMvc mockMvc;

  private String jsonRequestUpdateProduct;

  private ProductResponse productResponse;

  private ProductRequest productRequest;

  private Product product;

  private Item itemAForGetProductAndItems;

  private Item itemBForGetProductAndItems;

  private ArrayList<Item> items;

  private Product productForGetProductAndItems;

  private ProductAndItemsVO   productAndItemsVO;

  private ItemResponse itemAResponseForGet;

  private ItemResponse itemBResponseForGet;

  private ArrayList<ItemResponse> itemsResponseForGetProductAndItems;

  private ProductAndItemsResponse productAndItemsDTO;

  private SalesCatalogRequest salesCatalogRequest;

  private SalesCatalog salesCatalog;

  private String jsonRequestProductUpdateSalesCatalog;

  private SalesCatalogDTO salesCatalogResponse;

  private MasterCatalog masterCatalog;

  private MasterCatalogDTO masterCatalogResponse;

  private MasterCatalogRequest masterCatalogRequest;

  private String jsonRequestProductUpdateMasterCatalog;

  private String jsonRequestProductAttributeRequest;

  private String jsonRequestProductSkus;

  private String jsonRequestAddProduct;

  private ProductDTO productDTO;

  private String jsonRequestAddProductAndItems;

  private String jsonRequestProductAndItemActivationRequest;

  private ProductAndItemsRequest productAndItemsRequestDTO;

  private ProductAndItemsVO productAndItemsRequestVO;

  private ProductAttributeRequest productAttributeRequest;

  private MasterDataProductAttribute masterDataProductAttribute;

  private ProductCenterDetailResponse productCenterDetailResponse;

  private SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest;

  private CategoryBrandRequest categoryBrandRequest;

  private ProductBundleCreationRequest productBundleCreationRequest;

  private AddDeleteVariantRetryRequest addDeleteVariantRetryRequest;

  private final ObjectMapper mapper = new ObjectMapper();

  private PreOrderDTO preOrderDTO;
  private ProductL3Response productL3Response;
  private ProductItemsVo productItemsVo;

  @Test
  public void getSimpleProductByItemSkuCachedTest() throws Exception{
    List<String> itemSkus = Stream.of(ITEM_SKU).collect(toList());
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    SimpleProductResponse expectedResult = new SimpleProductResponse();
    expectedResult.setItemSku(ITEM_SKU);
    productAndItemsVO.getItems().add(itemAForGetProductAndItems);
    when(productService.getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus))).thenReturn(Stream.of(productAndItemsVO).collect(toList()));
    when(modelConverter.convertToSimpleProductResponse(productForGetProductAndItems,
        itemAForGetProductAndItems)).thenReturn(expectedResult);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_ITEM_SKUS_CACHED)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus));
    verify(modelConverter).convertToSimpleProductResponse(productAndItemsVO.getProduct(),
        productAndItemsVO.getItems().get(0));
  }

  @Test
  public void getSimpleProductByItemSkuCachedApplicationExceptionTest() throws Exception{
    List<String> itemSkus = Stream.of(ITEM_SKU).collect(toList());
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);

    productAndItemsVO.getItems().add(itemAForGetProductAndItems);
    when(productService.getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus))).thenThrow(new ApplicationRuntimeException());

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_ITEM_SKUS_CACHED)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.errorCode", equalTo("GET_SIMPLE_PRODUCT_FAILED")))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus));
  }

  @Test
  public void getSimpleProductByItemSkuCachedExceptionTest() throws Exception{
    List<String> itemSkus = Stream.of(ITEM_SKU).collect(toList());
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);

    productAndItemsVO.getItems().add(itemAForGetProductAndItems);
    when(productService.getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus))).thenThrow(new RuntimeException("Unspecified error :"));

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_ITEM_SKUS_CACHED)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.errorCode", equalTo("GET_SIMPLE_PRODUCT_FAILED")))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsByItemSkus(STORE_ID, REQUEST_ID, USERNAME,
        new HashSet<>(itemSkus));
  }

  @Test
  public void getProductInfoByItemSkuTest() throws Exception {
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    GetProductInfoRequest request = new GetProductInfoRequest();
    request.setItemSkus(itemSkus);
    request.setFullFetch(true);
    request.setPristine(true);
    request.setNeedWholesaleData(Boolean.TRUE);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    ProductAndItemInfoResponse expectedResult = new ProductAndItemInfoResponse();
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
            REQUEST_ID, USERNAME, USERNAME);
    when(productSearchService.getProductAndItemsInfoForActiveItem(mandatoryRequestParam, new HashSet<>(itemSkus),
        true, true, Boolean.TRUE, false, false)).thenReturn(Stream.of(productItemsVo).collect(toList()));
    when(modelConverter.convertToProductAndItemInfoResponse(productItemsVo))
        .thenReturn(expectedResult);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("combineOthersBundlings", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productSearchService).getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        new HashSet<>(itemSkus), true, true, Boolean.TRUE, false, false);
    verify(modelConverter).convertToProductAndItemInfoResponse(productItemsVo);
  }

  @Test
  public void getProductInfoByItemSkuExceptionTest() throws Exception{
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    GetProductInfoRequest request = new GetProductInfoRequest();
    request.setItemSkus(itemSkus);
    request.setFullFetch(true);
    request.setPristine(true);
    request.setNeedWholesaleData(Boolean.FALSE);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    MandatoryRequestParam mandatoryRequestParam =
            MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
                    REQUEST_ID, USERNAME, USERNAME);
    when(productSearchService.getProductAndItemsInfoForActiveItem(mandatoryRequestParam, new HashSet<>(itemSkus),
        true, true, Boolean.FALSE, false, false)).thenThrow(new RuntimeException("Unspecified error :"));

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("combineOthersBundlings", "true"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.errorCode", equalTo("GET_PRODUCT_INFO_FAILED")))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productSearchService).getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        new HashSet<>(itemSkus), true, true, Boolean.FALSE, false, false);
  }


  @Test
  public void getProductInfoByItemSkuApplicationExceptionTest() throws Exception{
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    GetProductInfoRequest request = new GetProductInfoRequest();
    request.setItemSkus(itemSkus);
    request.setFullFetch(true);
    request.setPristine(true);
    request.setNeedWholesaleData(Boolean.TRUE);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    MandatoryRequestParam mandatoryRequestParam =
            MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID,
                    REQUEST_ID, USERNAME, USERNAME);
    when(productSearchService.getProductAndItemsInfoForActiveItem(mandatoryRequestParam, new HashSet<>(itemSkus),
        true, true, Boolean.TRUE, false, false))
            .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED));

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("combineOthersBundlings", "true"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.errorCode", equalTo("GET_PRODUCT_INFO_FAILED")))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productSearchService).getProductAndItemsInfoForActiveItem(mandatoryRequestParam,
        new HashSet<>(itemSkus), true, true, Boolean.TRUE, false, false);
  }

  @Test
  public void addProductAndItemTest() throws Exception {
    when(this.modelConverter.convertToAddProductAndItemsResponse(ProductControllerTest.REQUEST_ID,
        ProductControllerTest.ADD_PRODUCT_AND_ITEM_RESPONSE_VO))
            .thenReturn(new GdnRestSingleResponse<AddProductAndItemsResponse>(null,
                ProductControllerTest.REQUEST_ID));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_AND_ITEMS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductAndItemActivationRequest)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.modelConverter).convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest());
    verify(this.modelConverter).convertToAddProductAndItemsResponse(
        ProductControllerTest.REQUEST_ID, ProductControllerTest.ADD_PRODUCT_AND_ITEM_RESPONSE_VO);
    verify(this.productService).addProductAndItems(eq(ProductControllerTest.STORE_ID),
        eq(ProductControllerTest.REQUEST_ID), eq(ProductControllerTest.USERNAME), Mockito.any(ProductItemsVo.class));
  }

  @Test
  public void addProductAndItemTestWithApplicationException() throws Exception {
    when(
        this.productService.addProductAndItems(eq(ProductControllerTest.STORE_ID), eq(ProductControllerTest.REQUEST_ID),
            eq(ProductControllerTest.USERNAME), Mockito.any(ProductItemsVo.class))).thenThrow(
        ApplicationRuntimeException.class);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_AND_ITEMS).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(this.jsonRequestProductAndItemActivationRequest)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk());
    verify(this.modelConverter).convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest());
    verify(this.productService).addProductAndItems(eq(ProductControllerTest.STORE_ID),
        eq(ProductControllerTest.REQUEST_ID), eq(ProductControllerTest.USERNAME), Mockito.any(ProductItemsVo.class));
  }

  @Test
  public void addProductAndItemTestWithException() throws Exception {
    when(this.productService
        .addProductAndItems(eq(ProductControllerTest.STORE_ID), eq(ProductControllerTest.REQUEST_ID),
            eq(ProductControllerTest.USERNAME), Mockito.any(ProductItemsVo.class))).thenThrow(Exception.class);
    this.mockMvc.perform(
            post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_AND_ITEMS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(this.jsonRequestProductAndItemActivationRequest)
                .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest());
    verify(this.productService).addProductAndItems(eq(ProductControllerTest.STORE_ID),
        eq(ProductControllerTest.REQUEST_ID), eq(ProductControllerTest.USERNAME), Mockito.any(ProductItemsVo.class));
  }

  @Test
  public void addProductSalesCatalogExceptionTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    when(this.productService.updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, null, ProductControllerTest.NEW_CATEGORY_CODE))
            .thenThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("newCategoryCode", ProductControllerTest.NEW_CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_SALES_CATEGORY.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, null, ProductControllerTest.NEW_CATEGORY_CODE);
  }

  @Test
  public void addProductSalesCatalogTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("newCategoryCode", ProductControllerTest.NEW_CATEGORY_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, null, ProductControllerTest.NEW_CATEGORY_CODE);
  }

  @Test
  public void addProductTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestAddProduct).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductDTOToProduct(this.productDTO);
    verify(this.productService).addProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, this.product);
  }

  @Test
  public void addProductTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestAddProduct).param("storeId", ProductControllerTest.BLANK)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductDTOToProduct(this.productDTO);
    verify(this.productService).addProduct(ProductControllerTest.BLANK,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, this.product);
  }

  @Test
  public void alterSalesCategorySequenceExceptionTest() throws Exception {
    List<SalesCategorySequence> salesCategorySequences = new ArrayList<>();
    List<SalesCategorySequenceRequest> salesCategorySequenceRequests =
        new ArrayList<>();

    salesCategorySequenceRequests.add(new SalesCategorySequenceRequest(
        ProductControllerTest.NEW_CATEGORY_CODE, ProductControllerTest.SEQUENCE));
    salesCategorySequences.add(new SalesCategorySequence(ProductControllerTest.NEW_CATEGORY_CODE,
        ProductControllerTest.SEQUENCE));

    when(this.modelConverter.convertRequestListToModel(salesCategorySequenceRequests,
        SalesCategorySequence.class)).thenReturn(salesCategorySequences);
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .alterSalesCategorySequence(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_SKU, salesCategorySequences);
    String body = this.mapper.writeValueAsString(salesCategorySequenceRequests);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ALTER_SALES_CATEGORY_SEQUENCE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ALTER_SALES_CATEGORY.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertRequestListToModel(salesCategorySequenceRequests,
        SalesCategorySequence.class);
    verify(this.productService).alterSalesCategorySequence(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU, salesCategorySequences);
  }

  @Test
  public void alterSalesCategorySequenceTest() throws Exception {
    List<SalesCategorySequence> salesCategorySequences = new ArrayList<>();
    List<SalesCategorySequenceRequest> salesCategorySequenceRequests =
        new ArrayList<>();

    salesCategorySequenceRequests.add(new SalesCategorySequenceRequest(
        ProductControllerTest.NEW_CATEGORY_CODE, ProductControllerTest.SEQUENCE));
    salesCategorySequences.add(new SalesCategorySequence(ProductControllerTest.NEW_CATEGORY_CODE,
        ProductControllerTest.SEQUENCE));

    when(this.modelConverter.convertRequestListToModel(salesCategorySequenceRequests,
        SalesCategorySequence.class)).thenReturn(salesCategorySequences);

    String body = this.mapper.writeValueAsString(salesCategorySequenceRequests);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ALTER_SALES_CATEGORY_SEQUENCE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertRequestListToModel(salesCategorySequenceRequests,
        SalesCategorySequence.class);
    verify(this.productService).alterSalesCategorySequence(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU, salesCategorySequences);
  }

  @Test
  public void deleteProductSalesCatalogExceptionTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    when(this.productService.updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE, null))
            .thenThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.DELETE_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("oldCategoryCode", ProductControllerTest.OLD_CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.DELETE_SALES_CATEGORY.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE, null);
  }

  @Test
  public void deleteProductSalesCatalogTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.DELETE_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("oldCategoryCode", ProductControllerTest.OLD_CATEGORY_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE, null);
  }

  @Test
  public void deleteProductTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.DELETE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).deleteProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU);
  }

  @Test
  public void deleteProductTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.DELETE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.DELETE_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).deleteProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU_INVALID);
  }

  @Test
  public void getProductAndItemsAvailabilityApplicationRuntimeExceptionTest() throws Exception {
    List<String> collect = Stream.of(PRODUCT_SKU).collect(toList());
    String body = this.mapper.writeValueAsString(new SimpleListStringRequest(collect));
    Map<String, List<ProductAndItemsVO>> value = new HashMap<>();
    ProductAndItemsVO productAndItemsVo = new ProductAndItemsVO();
    value.put(PRODUCT_CODE, Stream.of(productAndItemsVo).collect(toList()));

    when(this.productService.getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect))
            .thenThrow(new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ERROR_MESSAGE));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_AVAILABILITY)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unauthorize access :" + ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect);
  }

  @Test
  public void getProductAndItemsAvailabilityExceptionTest() throws Exception {
    List<String> collect = Stream.of(PRODUCT_SKU).collect(toList());
    String body = this.mapper.writeValueAsString(new SimpleListStringRequest(collect));
    Map<String, List<ProductAndItemsVO>> value = new HashMap<>();
    ProductAndItemsVO productAndItemsVo = new ProductAndItemsVO();
    value.put(PRODUCT_CODE, Stream.of(productAndItemsVo).collect(toList()));

    when(this.productService.getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect))
            .thenThrow(new RuntimeException(ERROR_MESSAGE));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_AVAILABILITY)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect);
  }

  @Test
  public void getProductAndItemsAvailabilityTest() throws Exception {
    List<String> collect = Stream.of(PRODUCT_SKU).collect(toList());
    String body = this.mapper.writeValueAsString(new SimpleListStringRequest(collect));
    Map<String, List<ProductAndItemsVO>> value = new HashMap<>();
    ProductAndItemsVO productAndItemsVo = new ProductAndItemsVO();
    value.put(PRODUCT_CODE, Stream.of(productAndItemsVo).collect(toList()));

    when(this.productService.getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect))
            .thenReturn(value);
    when(modelConverter.convertToProductAndItemsDTO(productAndItemsVo))
        .thenReturn(new ProductAndItemsResponse());
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_AVAILABILITY)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productService).getProductAndItemsAvailability(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, collect);
    verify(modelConverter).convertToProductAndItemsDTO(productAndItemsVo);
  }

  @Test
  public void getProductAndItemsByProductCatentryIdTest() throws Exception {
    when(this.productSearchService.getProductAndItemsByProductCatentryId(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID))
            .thenReturn(this.productAndItemsVO);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCatentryId", ProductControllerTest.PRODUCT_CATENTRY_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByProductCatentryId(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID);
    verify(this.modelConverter).convertToProductAndItemsDTO(this.productAndItemsVO);
  }

  @Test
  public void getProductAndItemsByProductCatentryIdWithApplicationExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED))
        .when(this.productSearchService).getProductAndItemsByProductCatentryId(
            ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCatentryId", ProductControllerTest.PRODUCT_CATENTRY_ID))
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCatentryId(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID);
  }

  @Test
  public void getProductAndItemsByProductCatentryIdWithRuntimeExceptionTest() throws Exception {
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE))
        .when(this.productSearchService).getProductAndItemsByProductCatentryId(
            ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_CATENTRY_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCatentryId", ProductControllerTest.PRODUCT_CATENTRY_ID))
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCatentryId(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CATENTRY_ID);
  }

  @Test
  public void getProductAndItemsByProductCodeTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    when(this.productSearchService.getProductAndItemsByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.PRODUCT_CODE, true)).thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(resultVo))
        .thenReturn(new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByProductCode(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, true);
    verify(this.modelConverter).convertToMasterDataDetailResponse(resultVo);
  }

  @Test
  public void getProductAndItemsByProductCodeWithApplicationExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductAndItemsByProductCode(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, true);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCode(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, true);
  }

  @Test
  public void getProductAndItemsByProductCodeWithRuntimeExceptionTest() throws Exception {
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE))
        .when(this.productSearchService).getProductAndItemsByProductCode(
            ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, true);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCode(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, true);
  }

  @Test
  public void getProductAndItemsSyncByProductCodesApplicationExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    doThrow(new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION)).when(
        this.productSearchService)
      .getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE, true, true,
        false);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("combineOthersBundlings", "true"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getMasterDataWithProductItemsVo(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, true, true, false);
  }

  @Test
  public void getProductAndItemsSyncByProductCodesRuntimeExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(
      this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUEST_ID,
        PRODUCT_CODE, true, false, false);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME,
      REQUEST_ID, PRODUCT_CODE, true, false, false);
  }

  @Test
  public void getProductAndItemsSyncByProductCodesSolrExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
      new MasterDataDetailWithProductAndItemsResponseVo();
    doThrow(SolrCustomException.class).when(
      this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUEST_ID,
      PRODUCT_CODE, true, false, false);
    this.mockMvc
      .perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE)
          .contentType(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.USERNAME)
          .param("productCode", ProductControllerTest.PRODUCT_CODE))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
      .andExpect(
        jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.value", equalTo(null)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME,
      REQUEST_ID, PRODUCT_CODE, true, false, false);
  }

  @Test
  public void getProductAndItemsSyncByProductCodesTest() throws Exception {
    MasterDataWithProductItemsVo resultVo =
        new MasterDataWithProductItemsVo();
    when(this.productSearchService.getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUEST_ID,
      PRODUCT_CODE, true, true, Boolean.parseBoolean(ProductControllerTest.OFF_2_ON))).thenReturn(
      resultVo);
    when(this.modelConverter.convertToMasterDataWithProductItemsVo(resultVo)).thenReturn(
      new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("combineOthersBundlings", "true")
                .param("off2On",ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME,
      REQUEST_ID, PRODUCT_CODE, true, true, Boolean.parseBoolean(ProductControllerTest.OFF_2_ON));
    verify(this.modelConverter).convertToMasterDataWithProductItemsVo(resultVo);
  }

  @Test
  public void getMasterDataAndProductAndItemDataTest() throws Exception {
    MasterDataDetailWithProductAndItemResponseVo resultVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    when(this.productSearchService
        .getMasterDataAndProductAndItemData(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE,  ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON)))
        .thenReturn(resultVo);
    when(this.modelConverter.toMasterDataDetailWithProductAndItemResponse(resultVo))
        .thenReturn(new MasterDataDetailWithProductAndItemResponse());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_MASTER_DATA_AND_PRODUCT_ITEM_DATA)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("itemSku", ProductControllerTest.ITEM_SKU)
                .param("combineOthersBundlings", "true")
                .param("off2On",ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getMasterDataAndProductAndItemData(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON));
    verify(this.modelConverter).toMasterDataDetailWithProductAndItemResponse(resultVo);
  }

  @Test
  public void getMasterDataAndProductAndItemDataApplicationRuntimeExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemResponseVo resultVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    when(this.productSearchService
        .getMasterDataAndProductAndItemData(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE,  ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON)))
        .thenReturn(resultVo);
    doThrow(new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION)).when(this.modelConverter)
        .toMasterDataDetailWithProductAndItemResponse(resultVo);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_MASTER_DATA_AND_PRODUCT_ITEM_DATA)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("itemSku", ProductControllerTest.ITEM_SKU)
                .param("combineOthersBundlings", "true")
                .param("off2On",ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.AUTHORIZATION.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getMasterDataAndProductAndItemData(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON));
    verify(this.modelConverter).toMasterDataDetailWithProductAndItemResponse(resultVo);
  }

  @Test
  public void getMasterDataAndProductAndItemDataRuntimeExceptionTestTest() throws Exception {
    MasterDataDetailWithProductAndItemResponseVo resultVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    when(this.productSearchService
        .getMasterDataAndProductAndItemData(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE,  ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON)))
        .thenReturn(resultVo);
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(this.modelConverter)
        .toMasterDataDetailWithProductAndItemResponse(resultVo);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_MASTER_DATA_AND_PRODUCT_ITEM_DATA)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("itemSku", ProductControllerTest.ITEM_SKU)
                .param("combineOthersBundlings", "true")
                .param("off2On",ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getMasterDataAndProductAndItemData(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRODUCT_CODE, ITEM_SKU,
            true, true, Boolean.valueOf(ProductControllerTest.OFF_2_ON));
    verify(this.modelConverter).toMasterDataDetailWithProductAndItemResponse(resultVo);
  }

  @Test
  public void getProductAndItemsSyncByProductCodesWithOff2OnTrueTest() throws Exception {
    MasterDataWithProductItemsVo resultVo =
        new MasterDataWithProductItemsVo();
    when(this.productSearchService.getMasterDataWithProductItemsVo(STORE_ID, USERNAME, REQUEST_ID,
      PRODUCT_CODE, true, true, true)).thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataWithProductItemsVo(resultVo)).thenReturn(
      new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_SYNC_BY_PRODUCT_CODE)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("productCode", ProductControllerTest.PRODUCT_CODE)
                .param("combineOthersBundlings", "true")
                .param("off2On","true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getMasterDataWithProductItemsVo(STORE_ID, USERNAME,
      REQUEST_ID, PRODUCT_CODE, true, true, true);
    verify(this.modelConverter).convertToMasterDataWithProductItemsVo(resultVo);
  }

  @Test
  public void getProductAndItemsTest() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU)
            .param("showDeleted", ProductControllerTest.SHOW_DELETED)
            .param("combineOthersBundlings", "true")
            .param("off2On", ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndItems(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true,
        Boolean.valueOf(ProductControllerTest.OFF_2_ON), true, false, false);
    verify(this.modelConverter).convertToProductAndItemsResponse(Mockito.any(ProductItemsVo.class), Mockito.isNull(String.class), Mockito.eq(false));
  }

  @Test
  public void getProductAndItemsNeedProductDataTest() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU)
            .param("showDeleted", ProductControllerTest.SHOW_DELETED)
            .param("needProductData", "true")
            .param("combineOthersBundlings", "true")
            .param("off2On", ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndItems(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true,
        Boolean.valueOf(ProductControllerTest.OFF_2_ON), true, false, false);
    verify(this.modelConverter).convertToProductAndItemsResponse(Mockito.any(ProductItemsVo.class), Mockito.isNull(String.class), Mockito.eq(false));
  }

  @Test
  public void getProductAndItemsIncludeForceReviewTest() throws Exception {
    when(this.productService.getProductAndItems(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true,
        Boolean.valueOf(ProductControllerTest.OFF_2_ON), true, true, false)).thenReturn(productItemsVo);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU)
            .param("showDeleted", ProductControllerTest.SHOW_DELETED)
            .param("combineOthersBundlings", "true")
            .param("includeForceReview","true")
            .param("off2On", ProductControllerTest.OFF_2_ON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndItems(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true,
        Boolean.valueOf(ProductControllerTest.OFF_2_ON), true, true, false);
    verify(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, false);
  }

  @Test
  public void getProductAndItemsWithApplicationException() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE))
        .when(this.productService).getProductAndItems(ProductControllerTest.STORE_ID,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.PRODUCT_SKU, false, false, false, true, false, false);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("showDeleted", ProductControllerTest.SHOW_DELETED))

        .andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ErrorCategory.COMMUNICATION_FAILURE.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).getProductAndItems(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, false, false, false, true, false, false);
  }

  @Test
  public void getProductAndItemsWithRuntimeException() throws Exception {
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .getProductAndItems(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true, false, true,
          false, false);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("showDeleted", ProductControllerTest.SHOW_DELETED)
        .param("combineOthersBundlings", "true"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).getProductAndItems(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, false, true, false, true, false, false);
  }

  @Test
  public void getProductAndSingleItemTest() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE)
            .param("convertPreOrderDetails", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false);
    verify(this.modelConverter).convertToProductAndItemsResponseWithConvertPreOrderDetails(
        productItemsVo, true, null, false);
  }

  @Test
  public void getProductAndSingleItemWithException() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.BLANK)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndSingleItemByItemSku(ProductControllerTest.BLANK,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false);
  }

  @Test
  public void getProductAndSingleItemWithRuntimeException() throws Exception {
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .getProductAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
            ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEMSKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false);
  }

  @Test
  public void getProductDetailAndSingleItemTest() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
    verify(this.modelConverter).convertToProductAndItemsDTO(this.productAndItemsVO);
  }

  @Test
  public void getProductDetailAndSingleItemWithException() throws Exception {
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.BLANK)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailAndSingleItemByItemSku(ProductControllerTest.BLANK,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
  }

  @Test
  public void getProductDetailAndSingleItemWithRuntimeException() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productService)
        .getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
            ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("itemSku", ProductControllerTest.ITEM_SKU)
            .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
            .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
  }

  @Test
  public void getProductDetailAndSingleItemWithSolrException() throws Exception {
    doThrow(SolrCustomException.class).when(this.productService)
      .getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
    this.mockMvc
      .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_AND_SINGLE_ITEM_BY_ITEMSKU)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("itemSku", ProductControllerTest.ITEM_SKU)
        .param("instantPickup", String.valueOf(ProductControllerTest.INSTANT_PICKUP))
        .param("pickupPointCode", ProductControllerTest.PICKUP_POINT_CODE))
      .andExpect(status().isOk()).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
      .andExpect(
        jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.value", nullValue()))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
      ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
      ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);
  }

  @Test
  public void getProductsWithoutSalesCatalogPerDayTest() throws Exception {
    ArrayList<ProductsToItemCatalogMapping> productsToItemCatalogMappings =
        new ArrayList<>();
    ProductsToItemCatalogMapping productsToItemCatalogMapping = new ProductsToItemCatalogMapping();
    productsToItemCatalogMappings.add(productsToItemCatalogMapping);
    PageRequest pageable = PageRequest.of(ProductControllerTest.PAGE, ProductControllerTest.SIZE);
    PageImpl<ProductsToItemCatalogMapping> pageImpl = new PageImpl<>(
        productsToItemCatalogMappings, pageable, ProductControllerTest.TOTAL);

    ArrayList<ProductsToItemCatalogMappingResponse> responses =
        new ArrayList<>();
    ProductsToItemCatalogMappingResponse response = new ProductsToItemCatalogMappingResponse();
    responses.add(response);

    when(this.modelConverter.convertListToResponse(productsToItemCatalogMappings,
        ProductsToItemCatalogMappingResponse.class)).thenReturn(responses);
    when(this.productSearchService.getProductAndItemWithoutSalesCatalogPerDay(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.FROM_DAY,
        ProductControllerTest.TO_DAY,
        ProductControllerTest.MONTH, ProductControllerTest.YEAR, PageRequest.of(0,
            ProductControllerTest.SIZE, Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE))))
                .thenReturn(pageImpl);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG_DAY_RANGE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductControllerTest.STORE_ID)
                .param("channelId", ProductControllerTest.CHANNEL_ID)
                .param("clientId", ProductControllerTest.CLIENT_ID)
                .param("requestId", ProductControllerTest.REQUEST_ID)
                .param("username", ProductControllerTest.USERNAME)
                .param("fromDay", String.valueOf(ProductControllerTest.FROM_DAY))
                .param("toDay", String.valueOf(ProductControllerTest.TO_DAY))
                .param("month", String.valueOf(ProductControllerTest.MONTH))
                .param("year", String.valueOf(ProductControllerTest.YEAR))
                .param("size", String.valueOf(ProductControllerTest.SIZE)))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemWithoutSalesCatalogPerDay(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.FROM_DAY,
        ProductControllerTest.TO_DAY, ProductControllerTest.MONTH, ProductControllerTest.YEAR,
        PageRequest.of(0, ProductControllerTest.SIZE,
            Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
    verify(this.modelConverter).convertListToResponse(productsToItemCatalogMappings,
        ProductsToItemCatalogMappingResponse.class);
  }

  @Test
  public void getProductsWithoutSalesCatalogTest() throws Exception {
    ArrayList<ProductsToItemCatalogMapping> productsToItemCatalogMappings =
        new ArrayList<>();
    ProductsToItemCatalogMapping productsToItemCatalogMapping = new ProductsToItemCatalogMapping();
    productsToItemCatalogMappings.add(productsToItemCatalogMapping);
    PageRequest pageable = PageRequest.of(ProductControllerTest.PAGE, ProductControllerTest.SIZE);
    PageImpl<ProductsToItemCatalogMapping> pageImpl = new PageImpl<>(
        productsToItemCatalogMappings, pageable, ProductControllerTest.TOTAL);

    ArrayList<ProductsToItemCatalogMappingResponse> responses =
        new ArrayList<>();
    ProductsToItemCatalogMappingResponse response = new ProductsToItemCatalogMappingResponse();
    responses.add(response);

    when(this.modelConverter.convertListToResponse(productsToItemCatalogMappings,
        ProductsToItemCatalogMappingResponse.class)).thenReturn(responses);
    when(this.productSearchService.getProductAndItemWithoutSalesCatalog(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID,
        ProductControllerTest.MONTH, ProductControllerTest.YEAR, PageRequest.of(0,
            ProductControllerTest.SIZE, Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE))))
                .thenReturn(pageImpl);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("month", String.valueOf(ProductControllerTest.MONTH))
            .param("year", String.valueOf(ProductControllerTest.YEAR))
            .param("size", String.valueOf(ProductControllerTest.SIZE)))
        .andExpect(status().isOk()).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.SIZE)))
        .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE)))
        .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.TOTAL)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemWithoutSalesCatalog(
        ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.MONTH, ProductControllerTest.YEAR,
        PageRequest.of(0, ProductControllerTest.SIZE,
            Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
    verify(this.modelConverter).convertListToResponse(productsToItemCatalogMappings,
        ProductsToItemCatalogMappingResponse.class);
  }

  @Test
  public void getProductsWithoutSalesCatalogExceptionTest() throws Exception {
    ArrayList<ProductsToItemCatalogMapping> productsToItemCatalogMappings =
      new ArrayList<>();
    ProductsToItemCatalogMapping productsToItemCatalogMapping = new ProductsToItemCatalogMapping();
    productsToItemCatalogMappings.add(productsToItemCatalogMapping);
    PageRequest pageable = PageRequest.of(ProductControllerTest.PAGE, ProductControllerTest.SIZE);
    PageImpl<ProductsToItemCatalogMapping> pageImpl = new PageImpl<>(
      productsToItemCatalogMappings, pageable, ProductControllerTest.TOTAL);

    ArrayList<ProductsToItemCatalogMappingResponse> responses =
      new ArrayList<>();
    ProductsToItemCatalogMappingResponse response = new ProductsToItemCatalogMappingResponse();
    responses.add(response);

    doThrow(new Exception("error")).when(this.productSearchService).getProductAndItemWithoutSalesCatalog(
      ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.REQUEST_ID,
      ProductControllerTest.MONTH, ProductControllerTest.YEAR, PageRequest.of(0,
        ProductControllerTest.SIZE, Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
    this.mockMvc
      .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("month", String.valueOf(ProductControllerTest.MONTH))
        .param("year", String.valueOf(ProductControllerTest.YEAR))
        .param("size", String.valueOf(ProductControllerTest.SIZE)))
      .andExpect(status().isOk()).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(jsonPath("$.errorCode", notNullValue()))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.content", notNullValue()))
      .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemWithoutSalesCatalog(
      ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.REQUEST_ID, ProductControllerTest.MONTH, ProductControllerTest.YEAR,
      PageRequest.of(0, ProductControllerTest.SIZE,
        Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
  }

  @Test
  public void getProductsWithoutSalesCatalogApplicationExceptionTest() throws Exception {
    ArrayList<ProductsToItemCatalogMapping> productsToItemCatalogMappings =
      new ArrayList<>();
    ProductsToItemCatalogMapping productsToItemCatalogMapping = new ProductsToItemCatalogMapping();
    productsToItemCatalogMappings.add(productsToItemCatalogMapping);
    PageRequest pageable = PageRequest.of(ProductControllerTest.PAGE, ProductControllerTest.SIZE);
    PageImpl<ProductsToItemCatalogMapping> pageImpl = new PageImpl<>(
      productsToItemCatalogMappings, pageable, ProductControllerTest.TOTAL);

    ArrayList<ProductsToItemCatalogMappingResponse> responses =
      new ArrayList<>();
    ProductsToItemCatalogMappingResponse response = new ProductsToItemCatalogMappingResponse();
    responses.add(response);

    doThrow(SolrCustomException.class).when(this.productSearchService).getProductAndItemWithoutSalesCatalog(
      ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.REQUEST_ID,
      ProductControllerTest.MONTH, ProductControllerTest.YEAR, PageRequest.of(0,
        ProductControllerTest.SIZE, Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
    this.mockMvc
      .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_WITHOUT_SALES_CATALOG)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("month", String.valueOf(ProductControllerTest.MONTH))
        .param("year", String.valueOf(ProductControllerTest.YEAR))
        .param("size", String.valueOf(ProductControllerTest.SIZE)))
      .andExpect(status().isOk()).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.content", notNullValue()))
      .andExpect(jsonPath("$.pageMetaData.pageSize", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.pageMetaData.pageNumber", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.pageMetaData.totalRecords", equalTo(ProductControllerTest.PAGE)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemWithoutSalesCatalog(
      ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.REQUEST_ID, ProductControllerTest.MONTH, ProductControllerTest.YEAR,
      PageRequest.of(0, ProductControllerTest.SIZE,
        Sort.by(Direction.ASC, SolrFieldNames.CREATED_DATE)));
  }


  @Test
  public void moveProductSalesCatalogExceptionTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    when(this.productService.updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE,
        ProductControllerTest.NEW_CATEGORY_CODE))
            .thenThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.MOVE_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("oldCategoryCode", ProductControllerTest.OLD_CATEGORY_CODE)
            .param("newCategoryCode", ProductControllerTest.NEW_CATEGORY_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.MOVE_SALES_CATEGORY.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE,
        ProductControllerTest.NEW_CATEGORY_CODE);
  }

  @Test
  public void moveProductSalesCatalogTest() throws Exception {
    List<String> productSkus = new ArrayList<>();
    productSkus.add(ProductControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(productSkus);
    String body = this.mapper.writeValueAsString(request);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.MOVE_SALES_CATEGORY)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(body).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("catalogCode", ProductControllerTest.CATALOG_CODE)
            .param("oldCategoryCode", ProductControllerTest.OLD_CATEGORY_CODE)
            .param("newCategoryCode", ProductControllerTest.NEW_CATEGORY_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).updateProductSalesCategory(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, productSkus,
        ProductControllerTest.CATALOG_CODE, ProductControllerTest.OLD_CATEGORY_CODE,
        ProductControllerTest.NEW_CATEGORY_CODE);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.productController).build();

    ObjectMapper mapper = new ObjectMapper();

    this.jsonRequestProductSkus =
        FileUtils.readFileToString(new File("src/test/resources/productSkus.json"));

    this.jsonRequestAddProduct =
        FileUtils.readFileToString(new File("src/test/resources/productDTO.json"));
    this.productDTO = mapper.readValue(this.jsonRequestAddProduct,
        mapper.getTypeFactory().constructType(ProductDTO.class));
    assertNotNull(this.productDTO);

    this.jsonRequestAddProductAndItems =
        FileUtils.readFileToString(new File("src/test/resources/productAndItems.json"));

    this.jsonRequestProductAndItemActivationRequest = mapper.writeValueAsString(new ProductAndItemActivationRequest());

    this.productAndItemsRequestDTO = mapper.readValue(this.jsonRequestAddProductAndItems,
        mapper.getTypeFactory().constructType(ProductAndItemsRequest.class));

    assertNotNull(this.productAndItemsRequestDTO);

    this.product = new Product();
    when(this.modelConverter.convertProductDTOToProduct(this.productDTO)).thenReturn(this.product);
    when(this.productService.addProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, this.product))
            .thenReturn(true);

    this.jsonRequestUpdateProduct =
        FileUtils.readFileToString(new File("src/test/resources/productRequest.json"));
    this.productRequest = mapper.readValue(this.jsonRequestUpdateProduct,
        mapper.getTypeFactory().constructType(ProductRequest.class));
    assertNotNull(this.productRequest);

    this.productResponse = new ProductResponse();

    when(this.modelConverter.convertToProduct(this.productRequest)).thenReturn(this.product);
    when(this.productService.addProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME, this.product))
            .thenReturn(true);
    when(this.modelConverter.convertToProductResponse(this.product))
        .thenReturn(this.productResponse);

    doThrow(new Exception(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .addProduct(ProductControllerTest.BLANK, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, this.product);

    this.itemAForGetProductAndItems = new Item();
    this.itemAForGetProductAndItems.setItemSku(ProductControllerTest.ITEM_SKU);

    this.itemBForGetProductAndItems = new Item();
    this.itemBForGetProductAndItems.setItemSku(ProductControllerTest.ITEM_SKU);
    this.itemBForGetProductAndItems.setItemCode(ProductControllerTest.ITEM_CODE);

    this.items = new ArrayList<>();
    this.items.add(this.itemAForGetProductAndItems);
    this.items.add(this.itemBForGetProductAndItems);

    this.productForGetProductAndItems = new Product();
    this.productForGetProductAndItems.setProductSku(ProductControllerTest.PRODUCT_SKU);
    productForGetProductAndItems.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));

    this.productAndItemsVO = new ProductAndItemsVO();
    this.productAndItemsVO.setProduct(this.productForGetProductAndItems);

    this.itemAResponseForGet = new ItemResponse();
    this.itemAResponseForGet.setItemSku(ProductControllerTest.ITEM_SKU);
    this.itemAResponseForGet.setItemCode(ProductControllerTest.ITEM_CODE);

    this.itemBResponseForGet = new ItemResponse();
    this.itemBResponseForGet.setItemSku(ProductControllerTest.ITEM_SKU);
    this.itemBResponseForGet.setItemCode(ProductControllerTest.ITEM_CODE);

    this.itemsResponseForGetProductAndItems = new ArrayList<>();
    this.itemsResponseForGetProductAndItems.add(this.itemAResponseForGet);
    this.itemsResponseForGetProductAndItems.add(this.itemBResponseForGet);

    this.productAndItemsDTO = new ProductAndItemsResponse();
    this.productResponse.setProductSku(ProductControllerTest.PRODUCT_SKU);
    this.productAndItemsDTO.setProduct(this.productResponse);

    this.salesCatalog = new SalesCatalog();
    this.salesCatalogResponse = new SalesCatalogDTO();

    this.masterCatalog = new MasterCatalog();
    this.masterCatalogResponse = new MasterCatalogDTO();

    this.masterDataProductAttribute = new MasterDataProductAttribute();

    this.productItemsVo = new ProductItemsVo();

    this.salesCategoryMappingUpdateRequest = new SalesCategoryMappingUpdateRequest();
    salesCategoryMappingUpdateRequest.setAddedCategories(Collections.emptyList());
    salesCategoryMappingUpdateRequest.setDeletedCategories(Collections.emptyList());

    this.jsonRequestProductUpdateMasterCatalog =
        FileUtils.readFileToString(new File("src/test/resources/masterCatalogRequest.json"));
    this.masterCatalogRequest = mapper.readValue(this.jsonRequestProductUpdateMasterCatalog,
        mapper.getTypeFactory().constructType(MasterCatalogRequest.class));
    assertNotNull(this.masterCatalogRequest);

    this.jsonRequestProductAttributeRequest =
        FileUtils.readFileToString(new File("src/test/resources/productAttributeRequest.json"));
    this.productAttributeRequest = mapper.readValue(this.jsonRequestProductAttributeRequest,
        mapper.getTypeFactory().constructType(ProductAttributeRequest.class));
    assertNotNull(this.productAttributeRequest);

    this.jsonRequestProductUpdateSalesCatalog =
        FileUtils.readFileToString(new File("src/test/resources/salesCatalogRequest.json"));
    this.salesCatalogRequest = mapper.readValue(this.jsonRequestProductUpdateSalesCatalog,
        mapper.getTypeFactory().constructType(SalesCatalogRequest.class));
    assertNotNull(this.salesCatalogRequest);

    this.productAndItemsRequestVO = new ProductAndItemsVO();

    when(this.productService.getProductAndItems(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, false, true, false, false, false, false)).thenReturn(this.productItemsVo);

    when(this.productService.getProductAndItems(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, false, true, false, true, false, false)).thenReturn(this.productItemsVo);

    when(this.modelConverter.convertToProductAndItemsDTO(this.productAndItemsVO))
        .thenReturn(this.productAndItemsDTO);
    when(this.modelConverter.convertToProductAndItemsResponse(
      Mockito.any(ProductItemsVo.class), Mockito.isNull(String.class), Mockito.eq(false)))
        .thenReturn(this.productAndItemsDTO);
    when(this.modelConverter.convertToProductAndItemsDTO(this.productAndItemsVO, true))
        .thenReturn(this.productAndItemsDTO);

    when(this.modelConverter.convertToProductAndItemsResponseWithConvertPreOrderDetails(
        Mockito.any(ProductItemsVo.class), Mockito.anyBoolean(), Mockito.isNull(String.class),
        eq(false))).thenReturn(this.productAndItemsDTO);

    doThrow(new ApplicationRuntimeException()).when(this.productService).getProductAndItems(
        ProductControllerTest.BLANK, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU, false, true, false, false,
      false, false);

    when(this.productService.getProductAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false))
            .thenReturn(this.productItemsVo);

    doThrow(new ApplicationRuntimeException()).when(this.productService)
        .getProductAndSingleItemByItemSku(ProductControllerTest.BLANK,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
            ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE, false);

    when(this.modelConverter.convertToMasterCatalog(this.masterCatalogRequest))
        .thenReturn(this.masterCatalog);
    doThrow(new ApplicationRuntimeException()).when(this.productService).updateProductMasterCatalog(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_INVALID,
        this.masterCatalog);
    doThrow(new ApplicationRuntimeException()).when(this.productService).updateProductMasterCatalogByProductSku(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_INVALID,
        this.masterCatalog);

    doThrow(new ApplicationRuntimeException()).when(this.productService).addProductAttribute(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_INVALID,
        this.masterDataProductAttribute);

    when(this.modelConverter.convertToSalesCatalog(this.salesCatalogRequest))
        .thenReturn(this.salesCatalog);
    when(this.modelConverter.convertToSalesCatalogResponse(this.salesCatalog))
        .thenReturn(this.salesCatalogResponse);
    when(this.productService.updateProductSalesCatalogByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.salesCatalog, true)).thenReturn(true);
    doThrow(new ApplicationRuntimeException()).when(this.productService).updateProductSalesCatalogByProductCode(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_INVALID,
        this.salesCatalog, true);
    doThrow(new ApplicationRuntimeException()).when(this.productService).updateProductSalesCatalogByProductSku(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_CODE_INVALID,
        this.salesCatalog, true);


    when(this.productService.deleteProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU)).thenReturn(true);
    doThrow(new ApplicationRuntimeException()).when(this.productService)
        .deleteProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU_INVALID);

    when(this.productService
        .updateProduct(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID, this.product, false))
        .thenReturn(product);

    doThrow(new ApplicationRuntimeException()).when(this.productService)
        .updateProduct(ProductControllerTest.BLANK, ProductControllerTest.REQUEST_ID, this.product, false);

    when(this.productService.synchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU)).thenReturn(productAndItemsVO);

    doThrow(new ApplicationRuntimeException()).when(this.productService).synchronizeProduct(
        ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU_INVALID);

    when(this.productService.unsynchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA))
            .thenReturn(productAndItemsVO);

    doThrow(new ApplicationRuntimeException()).when(this.productService).unsynchronizeProduct(
        ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU_INVALID,
        ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);

    when(this.modelConverter.convertToProductAndItemsRequestVO(any(ProductAndItemActivationRequest.class))).thenReturn(
        new ProductItemsVo());

    when(
        this.productService.addProductAndItems(eq(ProductControllerTest.STORE_ID), eq(ProductControllerTest.REQUEST_ID),
            eq(ProductControllerTest.USERNAME), eq(new ProductItemsVo()))).thenReturn(
        ProductControllerTest.ADD_PRODUCT_AND_ITEM_RESPONSE_VO);

    when(this.productService.getProductDetailAndSingleItemByItemSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
        ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE))
        .thenReturn(this.productAndItemsVO);

    doThrow(new ApplicationRuntimeException()).when(this.productService)
        .getProductDetailAndSingleItemByItemSku(ProductControllerTest.BLANK,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.ITEM_SKU, ProductControllerTest.NEED_MASTER_DATA_DETAIL,
            ProductControllerTest.INSTANT_PICKUP, ProductControllerTest.PICKUP_POINT_CODE);

    doNothing().when(this.productService).checkProductAndItemsForForceReview(Mockito.anyList(), Mockito.anyList());

    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderType(PREORDER_TYPE);
    preOrderDTO.setPreOrderValue(PREORDER_VALUE);

    productL3Response = new ProductL3Response();
    productL3Response.setProductCode(PRODUCT_CODE);

    categoryBrandRequest = new CategoryBrandRequest();

    List<String> category = new ArrayList<>();
    category.add(OLD_CATEGORY_CODE);
    category.add(NEW_CATEGORY_CODE);

    List<String> brand = new ArrayList<>();
    category.add(BRAND);

    categoryBrandRequest.setCategory(category);
    categoryBrandRequest.setBrand(brand);

    productBundleCreationRequest = new ProductBundleCreationRequest();
    List<BundleRecipeVo> bundleRecipes = new ArrayList<>();
    BundleRecipeVo bundleRecipe = new BundleRecipeVo();
    bundleRecipe.setItemSku(ITEM_SKU);
    bundleRecipe.setQuantity(5);
    bundleRecipes.add(bundleRecipe);
    productBundleCreationRequest.setBundleRecipeList(bundleRecipes);

    addDeleteVariantRetryRequest = new AddDeleteVariantRetryRequest();
  }

  @Test
  public void synchronizeProductTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.SYNCHRONIZE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProductAndItemsDTO(Mockito.any(ProductAndItemsVO.class));
    verify(this.productService).synchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU);
  }

  @Test
  public void synchronizeProductTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.SYNCHRONIZE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode",notNullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).synchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU_INVALID);
  }


  @Test
  public void synchronizeProductTestWithApplicationException() throws Exception {
    doThrow(new SolrCustomException("error")).when(this.productService).synchronizeProduct(STORE_ID,
      PRODUCT_SKU);
    this.mockMvc
      .perform(post(ProductApiPath.PRODUCT + ProductApiPath.SYNCHRONIZE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage",
        notNullValue()))
      .andExpect(
        jsonPath("$.errorCode",equalTo(ErrorCategory.UNSPECIFIED.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).synchronizeProduct(ProductControllerTest.STORE_ID,
      ProductControllerTest.PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productCenterHistoryService);
  }

  @Test
  public void unsynchronizeProductTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UNSYNCHRONIZE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU)
            .param("overwriteExistingMasterData",
                ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA + ""))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProductAndItemsDTO(Mockito.any(ProductAndItemsVO.class));

    verify(this.productService).unsynchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU, ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
  }

  @Test
  public void unsynchronizeProductTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UNSYNCHRONIZE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID)
            .param("overwriteExistingMasterData",
                ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA + ""))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(unsyncProductError)))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).unsynchronizeProduct(ProductControllerTest.STORE_ID,
        ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
        ProductControllerTest.PRODUCT_SKU_INVALID,
        ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
  }

  @Test
  public void unsynchronizeProductTestWithApplicationException() throws Exception {
    doThrow(new Exception("error")).when(this.productService).unsynchronizeProduct(
      ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
      ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU_INVALID,
      ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
    this.mockMvc
      .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UNSYNCHRONIZE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID)
        .param("overwriteExistingMasterData", String.valueOf(Boolean.TRUE)))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(
        jsonPath("$.errorCode", notNullValue()))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).unsynchronizeProduct(ProductControllerTest.STORE_ID,
      ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.PRODUCT_SKU_INVALID,
      ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
  }

  @Test
  public void unsynchronizeProductTestWithSolrException() throws Exception {
    doThrow(new SolrCustomException("error")).when(this.productService).unsynchronizeProduct(
      ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
      ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU_INVALID,
      ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
    this.mockMvc
      .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UNSYNCHRONIZE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID)
        .param("overwriteExistingMasterData", String.valueOf(Boolean.TRUE)))
      .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(
        jsonPath("$.errorCode", notNullValue()))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).unsynchronizeProduct(ProductControllerTest.STORE_ID,
      ProductControllerTest.REQUEST_ID, ProductControllerTest.USERNAME,
      ProductControllerTest.PRODUCT_SKU_INVALID,
      ProductControllerTest.OVERWRITE_EXISTING_MASTER_DATA);
  }

  @Test
  public void updateProductSalesCatalogTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATALOG)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateSalesCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToSalesCatalog(this.salesCatalogRequest);
    verify(this.productService).updateProductSalesCatalogByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.salesCatalog, true);
  }

  @Test
  public void updateProductSalesCatalogTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATALOG)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateSalesCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE_INVALID)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_SALES_CATALOG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToSalesCatalog(this.salesCatalogRequest);
    verify(this.productService).updateProductSalesCatalogByProductCode(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_INVALID, this.salesCatalog, true);
  }

  @Test
  public void updateProductMasterCatalogTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_MASTER_CATALOG)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateMasterCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToMasterCatalog(this.masterCatalogRequest);
    verify(this.productService).updateProductMasterCatalog(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.masterCatalog);
  }

  @Test
  public void updateProductMasterCatalogTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_MASTER_CATALOG)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateMasterCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE_INVALID)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_MASTER_CATALOG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToMasterCatalog(this.masterCatalogRequest);
    verify(this.productService).updateProductMasterCatalog(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_INVALID, this.masterCatalog);
  }

  @Test
  public void addProductAttributeTest() throws Exception {
    when(this.modelConverter.convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class))).thenReturn(this.masterDataProductAttribute);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_ATTRIBUTE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductAttributeRequest)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class));
    verify(this.productService).addProductAttribute(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.masterDataProductAttribute);
  }

  @Test
  public void addProductAttributeTestWithException() throws Exception {
    doThrow(new Exception(ProductControllerTest.ERROR_MESSAGE))
        .when(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
            any(ProductAttributeRequest.class));

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_ATTRIBUTE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductAttributeRequest)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productCode", ProductControllerTest.PRODUCT_CODE_INVALID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_PRODUCT_ATTRIBUTE.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class));
  }

  @Test
  public void addProductAttributeByProductSkuTest() throws Exception {
    when(this.modelConverter.convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class))).thenReturn(this.masterDataProductAttribute);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_ATTRIBUTE_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductAttributeRequest)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class));
    verify(this.productService).addProductAttributeByProductSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_SKU, this.masterDataProductAttribute);
  }

  @Test
  public void addProductAttributeByProductSkuTestWithException() throws Exception {
    doThrow(new Exception(ProductControllerTest.ERROR_MESSAGE))
        .when(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
            any(ProductAttributeRequest.class));

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.ADD_PRODUCT_ATTRIBUTE_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductAttributeRequest)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU_INVALID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ADD_PRODUCT_ATTRIBUTE.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertProductAttributeRequestToMasterDataProductAttribute(
        any(ProductAttributeRequest.class));
  }

  @Test
  public void publishAllProductsTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.PUBLISH_ALL_PRODUCTS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_PUBLISH_ALL_PRODUCTS),any(Runnable.class));
  }

  @Test
  public void republishProductsToAgpTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.REPUBLISH_PRODUCTS_TO_AGP)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductSkus)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.asyncProcessor).submitWithBackoff(eq(AgpConstant.COMMAND_REPUBLISH_PRODUCTS_TO_AGP),any(Runnable.class));
  }

  @Test
  public void updateProductTest() throws Exception {
    when(this.productService
        .updateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.any(Product.class), eq(false)))
        .thenReturn(product);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestUpdateProduct).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("isOnlyExternal", String.valueOf(false)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProduct(this.productRequest);
    verify(this.productService)
        .updateProduct(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID, this.product, false);
    verify(this.modelConverter).convertProductToProductResponse(this.product);
  }

  @Test
  public void updateProductTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestUpdateProduct).param("storeId", ProductControllerTest.BLANK)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("isOnlyExternal", String.valueOf(false)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProduct(this.productRequest);
    verify(this.productService)
        .updateProduct(ProductControllerTest.BLANK, ProductControllerTest.REQUEST_ID, this.product, false);
  }

  @Test
  public void getCountProductsByBrandNameTest() throws Exception {
    when(this.productService.getProductsCountByBrand(STORE_ID, ProductControllerTest.BRAND))
        .thenReturn(1L);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_BRAND)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("brand", ProductControllerTest.BRAND)).andExpect(status().isOk());
    verify(this.productService)
        .getProductsCountByBrand(ProductControllerTest.STORE_ID, ProductControllerTest.BRAND);
  }

  @Test
  public void getCountProductsByBrandNameWithNullProductTest() throws Exception {
    when(this.productService.getProductsCountByBrand(STORE_ID, ProductControllerTest.BRAND))
        .thenReturn(null);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_BRAND)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("brand", ProductControllerTest.BRAND)).andExpect(status().isOk());
    verify(this.productService)
        .getProductsCountByBrand(ProductControllerTest.STORE_ID, ProductControllerTest.BRAND);
  }

  @Test
  public void getCountProductsByBrandNameExceptionTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(this.productService).getProductsCountByBrand(STORE_ID, BRAND);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_BRAND)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("brand", ProductControllerTest.BRAND)).andExpect(status().isOk());
    verify(this.productService)
        .getProductsCountByBrand(ProductControllerTest.STORE_ID, ProductControllerTest.BRAND);
  }

  @Test
  public void getProductAndItemsByPristineId() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    when(this.productSearchService
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRISTINEID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("pristineId", "pristineId")
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
    verify(this.modelConverter).convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getProductAndItemsByPristineId_WhenApplicationRuntimeExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRISTINEID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("pristineId", "pristineId")
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode",
            equalTo(ProductErrorCodesEnum.GET_PRISTINE_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
  }

  @Test
  public void getProductAndItemsByPristineId_WhenExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    doThrow(new RuntimeException()).when(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, false,
            false);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRISTINEID)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME).param("pristineId", "pristineId")
            .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, false,
            false);
  }

  @Test
  public void getProductAndItemsByWrapper() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    when(this.skuValidator.isPristineId(PRISTINE_ID)).thenReturn(true);
    when(this.productSearchService
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, false,
            false)).thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("id", PRISTINE_ID)
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.skuValidator).isPristineId(PRISTINE_ID);
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, false,
            false);
    verify(this.modelConverter).convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getProductAndItemsByWrapper_WhenApplicationRuntimeExceptionTest()
      throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    when(this.skuValidator.isPristineId(PRISTINE_ID)).thenReturn(true);
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("id", PRISTINE_ID)
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.skuValidator).isPristineId(PRISTINE_ID);
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
  }

  @Test
  public void getProductAndItemsByWrapper_WhenExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    when(this.skuValidator.isPristineId(PRISTINE_ID)).thenReturn(true);
    doThrow(new RuntimeException()).when(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("id", PRISTINE_ID)
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.skuValidator).isPristineId(PRISTINE_ID);
    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, false, false);
  }

  @Test
  public void test_getProductAndItemsResponseByPristineId_AllSuccess() throws Exception {
    PristineProductAndItemsResponseVO resultVo =
          new PristineProductAndItemsResponseVO();
    PristineProductAndItemsResponse response =
          new PristineProductAndItemsResponse();
    when(this.productSearchService
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRISTINE_ID,
            Boolean.valueOf(ProductControllerTest.OFF_2_ON))).thenReturn(resultVo);
      when(this.modelConverter.convertToPristineProductAndItemsResponse(
          Mockito.any(PristineProductAndItemsResponseVO.class))).thenReturn(response);
      this.mockMvc.perform(get(ProductApiPath.PRODUCT
          + ProductApiPath.GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID)
          .contentType(MediaType.APPLICATION_JSON)
          .param("storeId", ProductControllerTest.STORE_ID)
          .param("channelId", ProductControllerTest.CHANNEL_ID)
          .param("clientId", ProductControllerTest.CLIENT_ID)
          .param("requestId", ProductControllerTest.REQUEST_ID)
          .param("username", ProductControllerTest.USERNAME)
          .param("id", ProductControllerTest.PRISTINE_ID)
          .param("off2On",ProductControllerTest.OFF_2_ON)).andExpect(status().isOk())
          .andExpect(jsonPath("$.errorMessage", nullValue()))
          .andExpect(jsonPath("$.errorCode", nullValue()))
          .andExpect(jsonPath("$.success", equalTo(true)))
          .andExpect(jsonPath("$.value", notNullValue()))
          .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.PRISTINE_ID,
            Boolean.valueOf(ProductControllerTest.OFF_2_ON));
      verify(this.modelConverter).convertToPristineProductAndItemsResponse(
          Mockito.any(PristineProductAndItemsResponseVO.class));

  }

  @Test
  public void test_getProductAndItemsResponseByPristineId_WithApplicationException() throws Exception {
    PristineProductAndItemsResponseVO resultVo =
        new PristineProductAndItemsResponseVO();
    PristineProductAndItemsResponse response =
        new PristineProductAndItemsResponse();
    when(this.productSearchService
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.PRISTINE_ID, false))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("id", ProductControllerTest.PRISTINE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.PRISTINE_ID, false);

  }

  @Test
  public void test_getProductAndItemsResponseByPristineId_WithOtherException() throws Exception {
    PristineProductAndItemsResponseVO resultVo =
        new PristineProductAndItemsResponseVO();
    PristineProductAndItemsResponse response =
        new PristineProductAndItemsResponse();
    when(this.productSearchService
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.PRISTINE_ID, false))
        .thenThrow(new RuntimeException());
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCTS_AND_ITEMS_RESPONSE_BY_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("id", ProductControllerTest.PRISTINE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getProductAndItemsResponseByPristineId(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.PRISTINE_ID, false);

  }


  @Test
  public void test_getPristineProductAndItemsInfoByItemSku_AllSuccess() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    PristineMasterDataDetailResponse response =
        new PristineMasterDataDetailResponse();
    when(this.productSearchService
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, true,
            Boolean.valueOf(ProductControllerTest.OFF_2_ON))).thenReturn(resultVo);
    when(this.modelConverter.convertToPristineMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("itemSku", ProductControllerTest.DEFAULT_SKU)
        .param("combineOthersBundlings", "true")
        .param("off2On",ProductControllerTest.OFF_2_ON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID, ProductControllerTest.USERNAME,
            ProductControllerTest.REQUEST_ID, ProductControllerTest.DEFAULT_SKU, true, true,
            Boolean.valueOf(ProductControllerTest.OFF_2_ON));
    verify(this.modelConverter).convertToPristineMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));

  }

  @Test
  public void test_getPristineProductAndItemsInfoByItemSku_WithApplicationException() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    PristineMasterDataDetailResponse response =
        new PristineMasterDataDetailResponse();
    when(this.productSearchService
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, true, false))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("itemSku", ProductControllerTest.DEFAULT_SKU)
        .param("combineOthersBundlings", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, true, false);

  }

  @Test
  public void test_getPristineProductAndItemsInfoByItemSku_WithOtherException() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    PristineMasterDataDetailResponse response =
        new PristineMasterDataDetailResponse();
    when(this.productSearchService
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, true, false))
        .thenThrow(new RuntimeException());

    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRISTINE_PRODUCT_AND_ITEMS_INFO_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME)
        .param("itemSku", ProductControllerTest.DEFAULT_SKU)
        .param("combineOthersBundlings", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productSearchService)
        .getPristineProductAndItemsInfoByItemSku(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.DEFAULT_SKU, true, true, false);

  }

  @Test
  public void getProductAndItemsByWrapper_WhenIdProductCodeTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    MasterDataDetailWithProductAndItemsResponse response =
        new MasterDataDetailWithProductAndItemsResponse();
    when(this.skuValidator.isPristineId("MTA-0308613")).thenReturn(false);
    when(this.productSearchService
        .getProductAndItemsSyncByProductCodes(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, "MTA-0308613", true, false, false))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT
        + ProductApiPath.GET_PRODUCT_AND_ITEMS_BY_PRODUCT_CODE_OR_PRISTINE_ID)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("id", "MTA-0308613")
        .param("defaultSku", ProductControllerTest.DEFAULT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.skuValidator).isPristineId("MTA-0308613");
    verify(this.productSearchService)
        .getProductAndItemsSyncByProductCodes(ProductControllerTest.STORE_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.REQUEST_ID, "MTA-0308613", true, false, false);
    verify(this.modelConverter).convertToMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void updateProductSalesCatalogByProductSkuTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATALOG_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateSalesCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_CODE)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToSalesCatalog(this.salesCatalogRequest);
    verify(this.productService).updateProductSalesCatalogByProductSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.salesCatalog, true);
  }

  @Test
  public void updateProductSalesCatalogByProductSkuTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATALOG_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateSalesCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_CODE_INVALID)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_SALES_CATALOG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToSalesCatalog(this.salesCatalogRequest);
    verify(this.productService).updateProductSalesCatalogByProductSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_INVALID, this.salesCatalog, true);
  }

  @Test
  public void updateProductMasterCatalogByProductSkuTest() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_MASTER_CATALOG_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateMasterCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToMasterCatalog(this.masterCatalogRequest);
    verify(this.productService).updateProductMasterCatalogByProductSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE, this.masterCatalog);
  }

  @Test
  public void updateProductMasterCatalogByProductSkuTestWithException() throws Exception {
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_MASTER_CATALOG_BY_PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(this.jsonRequestProductUpdateMasterCatalog)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_CODE_INVALID)
            .param("replace", ProductControllerTest.TRUE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_MASTER_CATALOG.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToMasterCatalog(this.masterCatalogRequest);
    verify(this.productService).updateProductMasterCatalogByProductSku(ProductControllerTest.STORE_ID,
        ProductControllerTest.PRODUCT_CODE_INVALID, this.masterCatalog);
  }

  @Test
  public void getProductDetailForReviewTest_WhenIdisProductSku() throws Exception {
    Mockito.when(skuValidator.isProductSku(ProductControllerTest.PRODUCT_SKU)).thenReturn(true);
    Mockito.when(this.productSearchService
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_SKU, ITEM_SKU)).thenReturn(new ReviewProductDetailVO());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("id", ProductControllerTest.PRODUCT_SKU)
            .param("itemSku", ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(skuValidator).isProductSku(ProductControllerTest.PRODUCT_SKU);
    Mockito.verify(this.productSearchService)
        .getProductDetailForReviewByProductSku(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_SKU, ITEM_SKU);
  }

  @Test
  public void getProductDetailForReviewTest_WhenIdisProductCode() throws Exception {
    Mockito.when(skuValidator.isProductSku(ProductControllerTest.PRODUCT_CODE)).thenReturn(false);
    Mockito.when(this.productSearchService
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU)).thenReturn(new ReviewProductDetailVO());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("id", ProductControllerTest.PRODUCT_CODE)
            .param("itemSku", ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(
            jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(skuValidator).isProductSku(ProductControllerTest.PRODUCT_CODE);
    Mockito.verify(this.productSearchService)
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU);
  }

  @Test
  public void getProductDetailForReviewTest_WhenException() throws Exception {
    Mockito.when(skuValidator.isProductSku(ProductControllerTest.PRODUCT_CODE)).thenReturn(false);
    Mockito.doThrow(new RuntimeException()).when(this.productSearchService)
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("id", ProductControllerTest.PRODUCT_CODE)
            .param("itemSku", ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(skuValidator).isProductSku(ProductControllerTest.PRODUCT_CODE);
    Mockito.verify(this.productSearchService)
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU);
  }

  @Test
  public void getProductDetailForReviewTest_WhenApplicationRuntimeException() throws Exception {
    Mockito.when(skuValidator.isProductSku(ProductControllerTest.PRODUCT_CODE)).thenReturn(false);
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU);
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAIL_FOR_REVIEW)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("id", ProductControllerTest.PRODUCT_CODE)
            .param("itemSku", ITEM_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_REVIEW_PRODUCT_DETAIL.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(skuValidator).isProductSku(ProductControllerTest.PRODUCT_CODE);
    Mockito.verify(this.productSearchService)
        .getProductDetailForReviewByProductCode(STORE_ID, USERNAME, REQUEST_ID,
            ProductControllerTest.PRODUCT_CODE, ITEM_SKU);
  }

  @Test
  public void toggleSuspensionProductTrueTest() throws Exception {
    Mockito.doNothing().when(this.productService)
        .toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
            ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.SUSPEND).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("suspendProduct", ProductControllerTest.SUSPENSION_TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null))).andExpect(jsonPath("$.errorMessage", Matchers
        .equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));



    verify(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);
  }

  @Test
  public void toggleSuspensionProductFalseTest() throws Exception {
    Mockito.doNothing().when(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_FALSE, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.SUSPEND).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("suspendProduct", ProductControllerTest.SUSPENSION_FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null))).andExpect(jsonPath("$.errorMessage", Matchers
        .equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    verify(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_FALSE, REQUEST_ID);
  }

  @Test
  public void toggleSuspensionProductTrueExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productService)
        .toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
            ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.SUSPEND).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("suspendProduct", ProductControllerTest.SUSPENSION_TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.SUSPEND_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    verify(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);
  }

  @Test
  public void toggleSuspensionProductTrueApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService)
        .toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
            ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.SUSPEND).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("suspendProduct", ProductControllerTest.SUSPENSION_TRUE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.SUSPEND_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    verify(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_TRUE, REQUEST_ID);
  }


  @Test
  public void toggleSuspensionProductFalseExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productService)
        .toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
            ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_FALSE, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.SUSPEND).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("productSku", ProductControllerTest.PRODUCT_SKU)
        .param("suspendProduct", ProductControllerTest.SUSPENSION_FALSE.toString())).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.SUSPEND_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    verify(this.productService).toggleSuspensionProduct(ProductControllerTest.STORE_ID, ProductControllerTest.PRODUCT_SKU,
        ProductControllerTest.USERNAME, ProductControllerTest.SUSPENSION_FALSE, REQUEST_ID);
  }

  @Test
  public void getProductsByProductCodeAndMerchantCodeTest() throws Exception {
    Mockito.when(this.productService.findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE))
        .thenReturn(Arrays.asList(new Product()));
    Mockito.when(this.modelConverter.convertToProductResponseList(Arrays.asList(new Product())))
        .thenReturn(Arrays.asList(new ProductResponse()));
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productCode", ProductControllerTest.PRODUCT_CODE)
        .param("merchantCode", ProductControllerTest.MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);
    Mockito.verify(modelConverter).convertToProductResponseList(Arrays.asList(new Product()));
  }

  @Test
  public void getProductsByProductCodeAndMerchantCode_withEmptyProductListTest() throws Exception {
    Mockito.when(this.productService.findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productCode", ProductControllerTest.PRODUCT_CODE)
        .param("merchantCode", ProductControllerTest.MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);
  }

  @Test
  public void getProductsByProductCodeAndMerchantCodeExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productCode", ProductControllerTest.PRODUCT_CODE)
        .param("merchantCode", ProductControllerTest.MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo("GET_SIMPLE_PRODUCT_FAILED")))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);
  }

  @Test
  public void getProductsByProductCodeAndMerchantCodeApplicationExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCTS_BY_PRODUCT_CODE_AND_MERCHANT_CODE)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productCode", ProductControllerTest.PRODUCT_CODE)
        .param("merchantCode", ProductControllerTest.MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.GET_SIMPLE_PRODUCT.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.productService)
        .findByStoreIdAndProductCodeAndMerchantCode(STORE_ID, PRODUCT_CODE, MERCHANT_CODE);
  }


  public void getUnmappedProductSkusTest() throws Exception {
    Mockito.when(productService.getUnmappedProductSkus(STORE_ID, new ArrayList<>())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.GET_UNMAPPED_SKU).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(mapper.writeValueAsString(new ArrayList<>()))
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.productService).getUnmappedProductSkus(STORE_ID, new ArrayList<>());
  }

  @Test
  public void getUnmappedProductSkusExceptionTest() throws Exception {
    Mockito.when(productService.getUnmappedProductSkus(STORE_ID, new ArrayList<>()))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.GET_UNMAPPED_SKU).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(mapper.writeValueAsString(new ArrayList<>()))
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.GET_UNMAPPED_PRODUCT_SKUS.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productService).getUnmappedProductSkus(STORE_ID, new ArrayList<>());
  }

  @Test
  public void getProductDetailsForProductCenterTest() throws Exception {
    when(this.productService.getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU, true))
        .thenReturn(productCenterDetailResponse);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue())).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService).getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void getProductDetailsForProductCenterExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productService).getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU,
        true);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.productService).getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU,
        true);
  }


  public void updateMigratedProductCodeTest() throws Exception {
    Mockito.doNothing().when(this.productService).updateMigratedProductCode(USERNAME, REQUEST_ID, STORE_ID, PRODUCT_SKU, PRODUCT_CODE, false);
    this.mockMvc.perform(put(ProductApiPath.PRODUCT + ProductApiPath.MIGRATE_PRODUCT_SKU)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("newProductCode", ProductControllerTest.PRODUCT_CODE)
        .param("productSku", ProductControllerTest.PRODUCT_SKU).param("rollback", "false")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    Mockito.verify(this.productService)
        .updateMigratedProductCode(USERNAME, REQUEST_ID, STORE_ID, PRODUCT_SKU, PRODUCT_CODE, false);
  }

  @Test
  public void updateMigratedProductCodeExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(productService)
        .updateMigratedProductCode(USERNAME, REQUEST_ID, STORE_ID, PRODUCT_SKU, PRODUCT_CODE, false);
    this.mockMvc.perform(put(ProductApiPath.PRODUCT + ProductApiPath.MIGRATE_PRODUCT_SKU)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("newProductCode", ProductControllerTest.PRODUCT_CODE)
        .param("productSku", ProductControllerTest.PRODUCT_SKU).param("rollback", "false")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.UPDATE_MIGRATED_PRODUCT_CODE.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.productService)
        .updateMigratedProductCode(USERNAME, REQUEST_ID, STORE_ID, PRODUCT_SKU, PRODUCT_CODE, false);
  }

  @Test
  public void getProductDetailsForProductCenterApplicationExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService)
        .getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU, true);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode",
        Matchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_DETAILS_FOR_PRODUCT_CENTER.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.productService).getProductDetailsForProductCenter(STORE_ID, PRODUCT_SKU,
        true);
  }

  @Test
  public void getProductCenterHistoryTest() throws Exception {
    Mockito.when(this.productCenterHistoryService
        .getProductCenterHistoryByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.PRODUCT_CENTER_HISTORY, PRODUCT_SKU)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    Mockito.verify(this.productCenterHistoryService)
        .getProductCenterHistoryByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, PAGE, SIZE);
  }

  @Test
  public void getProductCenterHistory_exceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productCenterHistoryService)
        .getProductCenterHistoryByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, PAGE, SIZE);

    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.PRODUCT_CENTER_HISTORY, PRODUCT_SKU)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE))).andExpect(status().isOk()).andExpect(jsonPath("$.errorCode",
        Matchers.equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    Mockito.verify(this.productCenterHistoryService)
        .getProductCenterHistoryByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, PAGE, SIZE);
  }

  @Test
  public void updateSalesCategoryTest() throws Exception {
    doNothing().when(productService).updateSalesCategory(STORE_ID, PRODUCT_SKU, new SalesCategoryMappingUpdateRequest(),
        REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATEGORY)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU)
        .content(mapper.writeValueAsString(salesCategoryMappingUpdateRequest)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    Mockito.verify(this.productService)
        .updateSalesCategory(eq(STORE_ID), eq(PRODUCT_SKU), any(SalesCategoryMappingUpdateRequest.class),
            eq(REQUEST_ID));
  }

  @Test
  public void updateSalesCategoryExceptionTest() throws Exception {
    doThrow(new Exception(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .updateSalesCategory(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_SKU, salesCategoryMappingUpdateRequest, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATEGORY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(mapper.writeValueAsString(salesCategoryMappingUpdateRequest))
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    Mockito.verify(this.productService)
        .updateSalesCategory(eq(STORE_ID), eq(PRODUCT_SKU), any(SalesCategoryMappingUpdateRequest.class),
            eq(REQUEST_ID));
  }

  @Test
  public void updateSalesCategoryApplicationExceptionTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(this.productService)
        .updateSalesCategory(ProductControllerTest.STORE_ID,
            ProductControllerTest.PRODUCT_SKU, salesCategoryMappingUpdateRequest, REQUEST_ID);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_SALES_CATEGORY)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(mapper.writeValueAsString(salesCategoryMappingUpdateRequest))
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_SALES_CATEGORY.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    Mockito.verify(this.productService)
        .updateSalesCategory(eq(STORE_ID), eq(PRODUCT_SKU), any(SalesCategoryMappingUpdateRequest.class),
            eq(REQUEST_ID));
  }

  @Test
  public void getL3CountByProductCodeTest() throws Exception {
    Mockito.when(this.productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn(1L);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_L3_COUNT_BY_PRODUCTCODE, PRODUCT_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("storeId", ProductControllerTest.STORE_ID)).andExpect(status().isOk());
    Mockito.verify(this.productService).getProductL3CountByProductCode(PRODUCT_CODE);
  }

  @Test
  public void getL3CountByProductCodeExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productService).getProductL3CountByProductCode(PRODUCT_CODE);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_L3_COUNT_BY_PRODUCTCODE, PRODUCT_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME)
        .param("storeId", ProductControllerTest.STORE_ID)).andExpect(status().isOk());
    Mockito.verify(this.productService).getProductL3CountByProductCode(PRODUCT_CODE);
  }

  @Test
  public void getPickupPointCodesByProductSkuTest() throws Exception {
    Mockito.when(this.productService.getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(new ProductPickupPointListResponse());
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PINPOINT_STATUS_BY_PRODUCT_SKU, PRODUCT_SKU)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService).getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getPickupPointCodesByProductSku_exceptionTest() throws Exception {
    Mockito.when(this.productService.getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU))
        .thenThrow(Exception.class);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PINPOINT_STATUS_BY_PRODUCT_SKU, PRODUCT_SKU)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.productService).getPickupPointCodesByProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getProductDetailsByProductSkuTest() throws Exception {
    when(productService.getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU)).thenReturn(productAndItemsVO);
    when(modelConverter.convertToProductL3Response(this.productAndItemsVO)).thenReturn(productL3Response);
    when(productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn(Long.valueOf(1));
    when(productService.getPromoLabels(Mockito.any(), Mockito.any())).thenReturn(new ArrayList<>());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    verify(this.modelConverter).convertToProductL3Response(this.productAndItemsVO);
    verify(this.productService).getProductL3CountByProductCode(Mockito.anyString());
    verify(this.productService).getPromoLabels(Mockito.any(ProductAndItemsVO.class), Mockito.any());
  }

  @Test
  public void getProductDetailsByProductSkuWithApplicationException() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE)).when(this.productService)
        .getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.COMMUNICATION_FAILURE.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_BY_PRODUCT_SKU.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.productService)
        .getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
  }

  @Test
  public void getProductDetailsByProductSkuWithRuntimeException() throws Exception {
    doThrow(new RuntimeException(ProductControllerTest.ERROR_MESSAGE)).when(this.productService)
        .getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(ProductControllerTest.ERROR_MESSAGE)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_BY_PRODUCT_SKU.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService)
        .getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
  }

  @Test
  public void getProductDetailsByProductSkuNonEditableTest() throws Exception {
    when(productService.getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU)).thenReturn(productAndItemsVO);
    when(modelConverter.convertToProductL3Response(this.productAndItemsVO)).thenReturn(productL3Response);
    when(productService.getProductL3CountByProductCode(PRODUCT_CODE)).thenReturn(Long.valueOf(2));
    when(productService.getPromoLabels(Mockito.any(), Mockito.any())).thenReturn(new ArrayList<>());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    verify(this.modelConverter).convertToProductL3Response(this.productAndItemsVO);
    verify(this.productService).getProductL3CountByProductCode(Mockito.anyString());
    verify(this.productService).getPromoLabels(Mockito.any(ProductAndItemsVO.class), Mockito.any());
  }

  @Test
  public void getProductDetailsByProductSkuNonEditableSharedProdutDetailsFromDbTest() throws Exception {
    ReflectionTestUtils.setField(this.productController, "sharedProductDetailsFromDb", true);
    when(productService.getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU)).thenReturn(productAndItemsVO);
    when(modelConverter.convertToProductL3Response(this.productAndItemsVO)).thenReturn(productL3Response);
    when(productService.isSharedProduct(STORE_ID, PRODUCT_CODE, false, productL3Response.getMerchantCode())).thenReturn(new SimpleBooleanResponse(true));
    when(productService.getPromoLabels(Mockito.any(), Mockito.any())).thenReturn(new ArrayList<>());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    verify(this.modelConverter).convertToProductL3Response(this.productAndItemsVO);
    verify(this.productService).isSharedProduct(STORE_ID, PRODUCT_CODE, false,
      productL3Response.getMerchantCode());
    verify(this.productService).getPromoLabels(Mockito.any(ProductAndItemsVO.class), Mockito.any());
  }

  @Test
  public void getProductDetailsByProductSkuEditableSharedProdutDetailsFromDbTest() throws Exception {
    ReflectionTestUtils.setField(this.productController, "sharedProductDetailsFromDb", true);
    when(productService.getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU)).thenReturn(productAndItemsVO);
    when(modelConverter.convertToProductL3Response(this.productAndItemsVO)).thenReturn(productL3Response);
    when(productService.isSharedProduct(STORE_ID, PRODUCT_CODE, false, productL3Response.getMerchantCode())).thenReturn(new SimpleBooleanResponse(false));
    when(productService.getPromoLabels(Mockito.any(), Mockito.any())).thenReturn(new ArrayList<>());
    this.mockMvc
        .perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_BY_PRODUCT_SKU)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)
            .param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).getProductDetailsByProductSku(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_SKU);
    verify(this.modelConverter).convertToProductL3Response(this.productAndItemsVO);
    verify(this.productService).isSharedProduct(STORE_ID, PRODUCT_CODE, false,
      productL3Response.getMerchantCode());
    verify(this.productService).getPromoLabels(Mockito.any(ProductAndItemsVO.class), Mockito.any());
  }

  @Test
  public void bulkUpdateOff2OnActiveFlagByProductSkusTest() throws Exception {
    Map<String, Boolean> stringBooleanMap = new HashMap<>();
    stringBooleanMap.put(PRODUCT_SKU, true);
    SimpleStringBooleanMapRequest request = new SimpleStringBooleanMapRequest(stringBooleanMap);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.BULK_UPDATE_OFF2ON_ACTIVE_FLAG_BY_PRODUCT_SKUS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productService)
        .updateOff2OnFlagByProductSkus(STORE_ID, request.getStringBooleanMap(), ProductControllerTest.USERNAME,
            ProductControllerTest.CHANNEL_ID, ProductControllerTest.CLIENT_ID, ProductControllerTest.REQUEST_ID, Boolean.FALSE);
  }

  @Test
  public void bulkUpdateOff2OnActiveFlagByProductSkusExceptionTest() throws Exception {
    Map<String, Boolean> stringBooleanMap = new HashMap<>();
    stringBooleanMap.put(PRODUCT_SKU, true);
    SimpleStringBooleanMapRequest request = new SimpleStringBooleanMapRequest(stringBooleanMap);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productService)
        .updateOff2OnFlagByProductSkus(STORE_ID, request.getStringBooleanMap(), USERNAME,
            ProductControllerTest.CHANNEL_ID, ProductControllerTest.CLIENT_ID, ProductControllerTest.REQUEST_ID, Boolean.FALSE);
    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.BULK_UPDATE_OFF2ON_ACTIVE_FLAG_BY_PRODUCT_SKUS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
        .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
        .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
        .param("username", ProductControllerTest.USERNAME).param("productSku", ProductControllerTest.PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value.value[0]", equalTo(PRODUCT_SKU)));
    Mockito.verify(this.productService)
        .updateOff2OnFlagByProductSkus(STORE_ID, request.getStringBooleanMap(), ProductControllerTest.USERNAME,
            ProductControllerTest.CHANNEL_ID, ProductControllerTest.CLIENT_ID, ProductControllerTest.REQUEST_ID, Boolean.FALSE);
  }

  @Test
  public void toggleArchiveProductTrueTest() throws Exception {
    Mockito.when(productService.toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.TRUE, Constants.SOURCE_PRODUCT_ARCHIVAL))
      .thenReturn(EditItemResponse.builder().isArchived(Boolean.FALSE).build());
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.PRODUCT_ARCHIVE, PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)
          .param("doArchive", Boolean.TRUE.toString())).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productService).toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.TRUE, Constants.SOURCE_PRODUCT_ARCHIVAL);
  }

  @Test
  public void toggleArchiveProductFalseTest() throws Exception {
    Mockito.when(productService.toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.FALSE,StringUtils.EMPTY))
      .thenReturn(EditItemResponse.builder().isArchived(Boolean.TRUE).build());
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.PRODUCT_ARCHIVE, PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)
          .param("doArchive", Boolean.FALSE.toString())).andExpect(status().isOk())
      .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
      .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productService).toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.FALSE, StringUtils.EMPTY);
  }

  @Test
  public void toggleArchiveProductTrueTestWithException() throws Exception {
    Mockito.doThrow(Exception.class).when(productService)
      .toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.TRUE, Constants.SOURCE_PRODUCT_ARCHIVAL);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.PRODUCT_ARCHIVE, PRODUCT_SKU).accept(
            MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
          .param("requestId", REQUEST_ID).param("username", USERNAME)
          .param("doArchive", Boolean.TRUE.toString())).andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.ARCHIVE_PRODUCT.getCode())))
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productService).toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, Boolean.TRUE, Constants.SOURCE_PRODUCT_ARCHIVAL);
  }

  @Test
  public void getProductCountByTypeTest() throws Exception {
    when(productWrapperService.getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, true))
        .thenReturn(new ProductCountResponseVo());
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("type", Constants.ACTIVE).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productWrapperService).getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, true);
  }

  @Test
  public void getProductCountByTypeExceptionTest() throws Exception {
    when(productWrapperService.getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, true))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_TYPE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("type", Constants.ACTIVE).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productWrapperService).getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, true);
  }

  @Test
  public void getSecondaryFilterCountsTest() throws Exception {
    when(productWrapperService.getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, false))
        .thenReturn(new ProductCountResponseVo());
    this.mockMvc.perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_SECONDARY_COUNTS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("type", Constants.ACTIVE).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productWrapperService).getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, false);
  }

  @Test
  public void getSecondaryFilterCountsExceptionTest() throws Exception {
    when(productWrapperService.getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, false))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
            get(ProductApiPath.PRODUCT + ProductApiPath.GET_SECONDARY_COUNTS).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("type", Constants.ACTIVE).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productWrapperService).getProductCountByType(Constants.ACTIVE, MERCHANT_CODE, false);
  }

  @Test
  public void getProductCountByCategoriesTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(categoryBrandRequest);
    Mockito.when(productService.getProductsCountByCategory(STORE_ID, categoryBrandRequest, MERCHANT_CODE))
        .thenReturn(PRODUCT_COUNT);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_CATEGORIES).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID).
            param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.value", Matchers.equalTo(PRODUCT_COUNT)))
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productService).getProductsCountByCategory(STORE_ID, categoryBrandRequest, MERCHANT_CODE);
  }

  @Test
  public void getProductCountByCategoriesTestWithException() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(categoryBrandRequest);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productService)
        .getProductsCountByCategory(STORE_ID, categoryBrandRequest, MERCHANT_CODE);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_COUNT_BY_CATEGORIES).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productService).getProductsCountByCategory(STORE_ID, categoryBrandRequest, MERCHANT_CODE);
  }

  @Test
  public void updateEditedProductTest() throws Exception {
    Mockito.when(
            this.productService.updateEditedProduct(Mockito.anyString(), Mockito.any(Product.class),
                Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
                Mockito.any()))
        .thenReturn(new EditProductDetailDTO());
    ProductEditRequest productEditRequest = new ProductEditRequest();
    productEditRequest.setItemViewConfigAndItemSkuListRequest(new ArrayList<>());
    productEditRequest.setProductRequest(productRequest);
    String requestBody = mapper.writeValueAsString(productEditRequest);
    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_EDITED_PRODUCT)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(requestBody).param("storeId", ProductControllerTest.STORE_ID)
            .param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID)
            .param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProduct(this.productRequest);
    verify(this.productService).updateEditedProduct(Mockito.anyString(), Mockito.any(Product.class),
        Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.isNull(),
        Mockito.any());
  }

  @Test
  public void updateEditedProduct1Test() throws Exception {
    when(modelConverter.convertToItemViewConfigMap(Mockito.anyList())).thenReturn(new HashMap());
    Mockito.when(
            this.productService.updateEditedProduct(Mockito.anyString(), Mockito.any(Product.class),
                Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
                Mockito.any()))
        .thenReturn(new EditProductDetailDTO());
    ProductEditRequest productEditRequest = new ProductEditRequest();
    productEditRequest.setItemViewConfigAndItemSkuListRequest(Arrays.asList(new ItemViewConfigAndItemSkuRequest()));
    productEditRequest.setProductRequest(null);
    String requestBody = mapper.writeValueAsString(productEditRequest);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_EDITED_PRODUCT).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToItemViewConfigMap(Mockito.anyList());
    verify(this.productService).updateEditedProduct(Mockito.anyString(), Mockito.isNull(),
        Mockito.anyMap(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void updateEditedProductTestWithException() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(modelConverter).convertToProduct(Mockito.any(ProductRequest.class));
    ProductEditRequest productEditRequest = new ProductEditRequest();
    productEditRequest.setItemViewConfigAndItemSkuListRequest(new ArrayList<>());
    productEditRequest.setProductRequest(productRequest);
    String requestBody = mapper.writeValueAsString(productEditRequest);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT + ProductApiPath.UPDATE_EDITED_PRODUCT).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk()).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.UPDATE_PRODUCT_SPECIAL_ATTRIBUTES_FAILED.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));

    verify(this.modelConverter).convertToProduct(this.productRequest);
  }

  @Test
  public void activateOnNeedCorrectionExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productService)
        .activateProductAndItemsOnNeedCorrection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any(NeedCorrectionProductActivationRequest.class));
    String requestBody = mapper.writeValueAsString(new NeedCorrectionProductActivationRequest());
    this.mockMvc.perform(
        put(ProductApiPath.PRODUCT + ProductApiPath.ACTIVATE_NEED_CORRECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.ACTIVATE_PRODUCT_NEED_CORRECTION.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).activateProductAndItemsOnNeedCorrection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(NeedCorrectionProductActivationRequest.class));
  }

  @Test
  public void activateOnNeedCorrectionTest() throws Exception {
    Mockito.when(this.productService.activateProductAndItemsOnNeedCorrection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(NeedCorrectionProductActivationRequest.class))).thenReturn(new ActivateNeedRevisionResponse());
    String requestBody = mapper.writeValueAsString(new NeedCorrectionProductActivationRequest());
    this.mockMvc.perform(
        put(ProductApiPath.PRODUCT + ProductApiPath.ACTIVATE_NEED_CORRECTION).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService).activateProductAndItemsOnNeedCorrection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(NeedCorrectionProductActivationRequest.class));
  }

  @Test
  public void retryPublishSchedulerTest() throws Exception {
    doNothing().when(productRetryEventPublishService).schedulerRetryPublish(STORE_ID);
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.SCHEDULER_TO_RETRY_PUBLISH).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("type", Constants.ACTIVE).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productRetryEventPublishService).schedulerRetryPublish(STORE_ID);
  }

  @Test
  public void getProductTypeByProductCodeTest() throws Exception {
    when(productService.getProductTypeByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_CODE)).thenReturn(new ProductTypeResponse());
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_TYPE_BY_PRODUCT_CODE, PRODUCT_CODE)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService)
        .getProductTypeByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_CODE);
  }

  @Test
  public void getProductTypeByProductCodeExceptionTest() throws Exception {
    when(productService.getProductTypeByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
        ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_CODE)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.GET_PRODUCT_TYPE_BY_PRODUCT_CODE, PRODUCT_CODE)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductControllerTest.STORE_ID)
        .param("channelId", ProductControllerTest.CHANNEL_ID).param("clientId", ProductControllerTest.CLIENT_ID)
        .param("requestId", ProductControllerTest.REQUEST_ID).param("username", ProductControllerTest.USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_TYPE_BY_PRODUCT_CODE.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(this.productService)
        .getProductTypeByProductCode(ProductControllerTest.STORE_ID, ProductControllerTest.REQUEST_ID,
            ProductControllerTest.USERNAME, ProductControllerTest.PRODUCT_CODE);
  }

  @Test
  public void deleteArchivedProductDataTest() throws Exception {
    Mockito.doNothing().when(productDataArchivalService).deleteProductArchivedData(ProductControllerTest.STORE_ID);
    this.mockMvc.perform(get(ProductApiPath.PRODUCT + ProductApiPath.DELETE_ARCHIVED_PRODUCT_DATA).param("storeId",
                ProductControllerTest.STORE_ID).param("channelId", ProductControllerTest.CHANNEL_ID)
            .param("clientId", ProductControllerTest.CLIENT_ID).param("requestId", ProductControllerTest.REQUEST_ID)
            .param("username", ProductControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductControllerTest.REQUEST_ID)));
    verify(productDataArchivalService).deleteProductArchivedData(ProductControllerTest.STORE_ID);
  }

  @Test
  public void makeProductBundleTest() throws Exception {
    String requestBody = mapper.writeValueAsString(productBundleCreationRequest);
    Mockito.doNothing().when(productService).makeProductBundleFromBundleRecipe(STORE_ID, ITEM_SKU, productBundleCreationRequest);
    this.mockMvc.perform(
            post(ProductApiPath.PRODUCT +  ProductApiPath.MAKE_PRODUCT_BUNDLE, ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(this.productService).makeProductBundleFromBundleRecipe(STORE_ID, ITEM_SKU, productBundleCreationRequest);
  }

  @Test
  public void makeProductBundleExceptionTest() throws Exception {
    String requestBody = mapper.writeValueAsString(productBundleCreationRequest);
    Mockito.doThrow(Exception.class).when(productService)
        .makeProductBundleFromBundleRecipe(STORE_ID, ITEM_SKU, productBundleCreationRequest);
    this.mockMvc.perform(
            post(ProductApiPath.PRODUCT + ProductApiPath.MAKE_PRODUCT_BUNDLE, ITEM_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ProductErrorCodesEnum.BUNDLE_CREATION_FAILED.getCode())))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(this.productService).makeProductBundleFromBundleRecipe(STORE_ID, ITEM_SKU, productBundleCreationRequest);
  }

  @Test
  public void isSharedProductTest() throws Exception {
    Mockito.when(productService.isSharedProduct(STORE_ID, PRODUCT_CODE, false, MERCHANT_CODE)).thenReturn(new SimpleBooleanResponse(true));
    this.mockMvc.perform(
        get(ProductApiPath.PRODUCT + ProductApiPath.SHARED_PRODUCT.replace("{productCode}",
          PRODUCT_CODE).replace("{sellerCode}",MERCHANT_CODE))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CHANNEL_ID)).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(productService).isSharedProduct(STORE_ID, PRODUCT_CODE, false, MERCHANT_CODE);
  }

  @Test
  public void reconcileProductVariantsTest() throws Exception {
    String requestBody = mapper.writeValueAsString(addDeleteVariantRetryRequest);
    Mockito.when(productService.reconcileProductVariants(STORE_ID, REQUEST_ID, USERNAME, addDeleteVariantRetryRequest))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.RECONCILE_PRODUCT_VARIANTS, PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    Mockito.verify(productService)
        .reconcileProductVariants(STORE_ID, REQUEST_ID, USERNAME, addDeleteVariantRetryRequest);
  }

  @Test
  public void reconcileProductVariantsErrorTest() throws Exception {
    String requestBody = mapper.writeValueAsString(addDeleteVariantRetryRequest);
    Mockito.when(productService.reconcileProductVariants(STORE_ID, REQUEST_ID, USERNAME, addDeleteVariantRetryRequest))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ProductApiPath.PRODUCT + ProductApiPath.RECONCILE_PRODUCT_VARIANTS, PRODUCT_SKU).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    Mockito.verify(productService)
        .reconcileProductVariants(STORE_ID, REQUEST_ID, USERNAME, addDeleteVariantRetryRequest);
  }

  @Test
  public void isPromoItemAvailableExceptionTest() throws Exception {
    PromoEligibilityResponse promoEligibilityResponse = new PromoEligibilityResponse();
    promoEligibilityResponse.setPromoEligibility(Map.of("SKU123", true));
    PromoEligibilityRequest promoEligibilityRequest = new PromoEligibilityRequest();
    promoEligibilityRequest.setBusinessPartnerAndProductSkuList(
        Map.of("merchantCode", Set.of("SKU123")));
    when(productService.isPromoItemAvailable(STORE_ID,
        promoEligibilityRequest.getBusinessPartnerAndProductSkuList())).thenReturn(
        promoEligibilityResponse);
    this.mockMvc.perform(post(ProductApiPath.PRODUCT
            + ProductApiPath.GET_CAMPAIGN_STATUS_BY_BUSINESS_PARTNER_PRODUCT_SKU_MAP).param(
                "storeId",
                STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(promoEligibilityRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(productService).isPromoItemAvailable(STORE_ID,
        promoEligibilityRequest.getBusinessPartnerAndProductSkuList());
  }

}
