package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.exception.SolrCustomException;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.ProductWrapper;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductListControllerTest {


  private static final String PAGE = "0";

  private static final String SIZE = "10";

  private static final Pageable GENERATE_PAGEABLE = PageRequest.of(0, 10);

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String MERCHANT_CODE = "dummy-merchant-code";

  private static final String BRAND_CODE = "dummy-brand-code";

  private static final String PRODUCT_SKU = "dummy-product-sku";

  private static final String PRODUCT_CODE = "dummy-product-code";

  private static final String PRODUCT_NAME = "dummy-product-name";

  private static final String STATUS = "Suspended";

  @InjectMocks
  private ProductListController productController;

  @Mock
  private ProductService productService;

  @Mock
  private ObjectMapper objectMapperMock;

  @Mock
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private ModelConverter modelConverter;


  private MockMvc mockMvc;

  private ObjectMapper objectMapper;

  private String simpleSetRequestJson;
  private SimpleSetStringRequest simpleSetRequest;
  private String productWrapperJson;
  private String pristineIdsJson;
  private ProductWrapper productWrapper;
  private Set<String> pristineIds;
  private OfficialStoreRequestVO officialStoreRequestVO = new OfficialStoreRequestVO();
  private ProductDetailRequest productDetailRequest = new ProductDetailRequest();
  private ActiveProductsRequestVO activeProductsRequestVO = new ActiveProductsRequestVO();
  private ActiveProductRequest activeProductRequest = new ActiveProductRequest();

  @Test
  public void getAllProductAndItemsSortByProductCodeAscTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    when(
        this.productSearchService
            .getAllProductAndItemsSortByProductCodeAsc(ProductListControllerTest.STORE_ID,
                ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID, true,
                ProductListControllerTest.GENERATE_PAGEABLE)).thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(resultVo)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_ALL)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("size", ProductListControllerTest.SIZE)
                .param("page", ProductListControllerTest.PAGE)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getAllProductAndItemsSortByProductCodeAsc(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, true, ProductListControllerTest.GENERATE_PAGEABLE);
    verify(this.modelConverter).convertToMasterDataDetailResponse(resultVo);
  }

  @Test
  public void getAllProductAndItemsSortByProductCodeAscTestWithSolrExceptionTest() throws Exception {
    doThrow(SolrCustomException.class).when(this.productSearchService)
        .getAllProductAndItemsSortByProductCodeAsc(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID, true,
            ProductListControllerTest.GENERATE_PAGEABLE);
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_ALL)
                .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("size", ProductListControllerTest.SIZE)
                .param("page", ProductListControllerTest.PAGE)
                .param("username", ProductListControllerTest.USERNAME))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getAllProductAndItemsSortByProductCodeAsc(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, true, ProductListControllerTest.GENERATE_PAGEABLE);
  }

  @Test
  public void getAllProductAndItemsSortByProductCodeAscTestWithExceptionTest() throws Exception {
    doThrow(new Exception("error")).when(this.productSearchService)
      .getAllProductAndItemsSortByProductCodeAsc(ProductListControllerTest.STORE_ID,
        ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID, true,
        ProductListControllerTest.GENERATE_PAGEABLE);
    this.mockMvc
      .perform(
        get(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_ALL)
          .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
          .param("storeId", ProductListControllerTest.STORE_ID)
          .param("channelId", ProductListControllerTest.CHANNEL_ID)
          .param("clientId", ProductListControllerTest.CLIENT_ID)
          .param("requestId", ProductListControllerTest.REQUEST_ID)
          .param("size", ProductListControllerTest.SIZE)
          .param("page", ProductListControllerTest.PAGE)
          .param("username", ProductListControllerTest.USERNAME))
      .andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.value", nullValue()))
      .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getAllProductAndItemsSortByProductCodeAsc(
      ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
      ProductListControllerTest.REQUEST_ID, true, ProductListControllerTest.GENERATE_PAGEABLE);
  }

  @Test
  public void getProductAndItemsByProductCatentryIdsTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    when(
        this.productSearchService.getProductAndItemsByProductCatentryIds(
            ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
            ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(resultVo)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByProductCatentryIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
    verify(this.modelConverter).convertToMasterDataDetailResponse(resultVo);
  }

  @Test
  public void getProductAndItemsByProductCatentryIdsWithExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductAndItemsByProductCatentryIds(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue(), true);
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CATENTRY_IDS)
                .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .content(this.simpleSetRequestJson)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCatentryIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
  }


  @Test
  public void getProductAndItemsByProductCodesTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    when(
        this.productSearchService.getProductAndItemsByProductCodes(
            ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
            ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToMasterDataDetailResponse(resultVo)).thenReturn(
        new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CODES)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByProductCodes(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
    verify(this.modelConverter).convertToMasterDataDetailResponse(resultVo);
  }

  @Test
  public void getProductAndItemsByProductCodesWithExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductAndItemsByProductCodes(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue(), true);
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_CODES)
                .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductCodes(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
  }


  @Test
  public void getProductAndItemsByProductSkusTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    when(
        this.productSearchService.getProductAndItemsByProductSkus(
            ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
            ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true))
        .thenReturn(CommonUtil.toMasterDataWithProductItemsVo(resultVo));
    when(this.modelConverter.convertToMasterDataWithProductItemsVo(Mockito.any())).thenReturn(
        new MasterDataDetailWithProductAndItemsResponse());
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_SKUS)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByProductSkus(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
    verify(this.modelConverter).convertToMasterDataWithProductItemsVo(Mockito.any());
  }

  @Test
  public void getProductAndItemsByProductSkusWithExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductAndItemsByProductSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue(), true);
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_SKUS)
                .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .content(this.simpleSetRequestJson)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));

    verify(this.productSearchService).getProductAndItemsByProductSkus(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.simpleSetRequest.getValue(), true);
  }


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.productController).build();

    objectMapper = new ObjectMapper();

    this.simpleSetRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuList.json"));
    this.simpleSetRequest =
        objectMapper.readValue(this.simpleSetRequestJson,
            objectMapper.getTypeFactory()
                .constructType(SimpleSetStringRequest.class));

    this.productWrapperJson = FileUtils.readFileToString(new File("src/test/resources/productWrapper.json"));
    this.productWrapper = objectMapper.readValue(this.productWrapperJson,
        objectMapper.getTypeFactory()
            .constructType(ProductWrapper.class));

    this.pristineIds = new HashSet();
    this.pristineIds.add("PRISTINE_ID");
    this.pristineIdsJson = objectMapper.writeValueAsString(pristineIds);

    officialStoreRequestVO.setProductSku(PRODUCT_SKU);
    officialStoreRequestVO.setMerchantCodes(Collections.singletonList(MERCHANT_CODE));
    officialStoreRequestVO.setProductName(PRODUCT_NAME);
    officialStoreRequestVO.setBrands(Collections.singletonList(BRAND_CODE));

    productDetailRequest.setBrands(Collections.singletonList(BRAND_CODE));
    productDetailRequest.setMerchantCodes(Collections.singletonList(MERCHANT_CODE));
    productDetailRequest.setProductName(PRODUCT_NAME);
    productDetailRequest.setProductSku(PRODUCT_SKU);
  }

  @Test
  public void getProductAndItemsByProductWrapperTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<SimpleProductsAndItemsResponse> response = new ArrayList<SimpleProductsAndItemsResponse>();
    when(this.productSearchService
        .getProductAndItemsByPristineIds(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getPristineIds())).thenReturn(resultVo);
    when(this.modelConverter.convertToSimpleProductsAndItemsDTO(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER)
            .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductListControllerTest.STORE_ID)
            .param("channelId", ProductListControllerTest.CHANNEL_ID)
            .param("clientId", ProductListControllerTest.CLIENT_ID)
            .param("requestId", ProductListControllerTest.REQUEST_ID)
            .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.errorCode", equalTo("")))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getProductAndItemsByPristineIds(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getPristineIds());
    verify(this.modelConverter).convertToSimpleProductsAndItemsDTO(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getProductAndItemsByProductWrapper_WhenPristineIdNullTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    ObjectMapper mapper = new ObjectMapper();
    String productWrapperJson1 =
        FileUtils.readFileToString(new File("src/test/resources/productWrapper2.json"));
    ProductWrapper productWrapper1 = mapper.readValue(this.productWrapperJson,
        mapper.getTypeFactory().constructType(ProductWrapper.class));

    List<SimpleProductsAndItemsResponse> response = new ArrayList<SimpleProductsAndItemsResponse>();
    when(this.productSearchService
        .getProductAndItemsByProductCodesAndProductSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            productWrapper1.getProductCodes(), productWrapper1.getProductSkus()))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToSimpleProductsAndItemsDTO(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(
        post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER)
            .content(productWrapperJson1).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).param("storeId", ProductListControllerTest.STORE_ID)
            .param("channelId", ProductListControllerTest.CHANNEL_ID)
            .param("clientId", ProductListControllerTest.CLIENT_ID)
            .param("requestId", ProductListControllerTest.REQUEST_ID)
            .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.errorCode", equalTo("")))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getProductAndItemsByProductCodesAndProductSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            productWrapper1.getProductCodes(), productWrapper1.getProductSkus());
    verify(this.modelConverter).convertToSimpleProductsAndItemsDTO(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getProductAndItemsByProductWrapperExceptionTest() throws Exception {
    doThrow(new RuntimeException()).when(
        this.productSearchService).getProductAndItemsByPristineIds(
            ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
            ProductListControllerTest.REQUEST_ID, this.productWrapper.getPristineIds());
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER)
                .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage",equalTo( ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo( ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.productWrapper.getPristineIds());
  }

  @Test
  public void getProductAndItemsByProductWrapperApplicationRuntimeExceptionTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<SimpleProductsAndItemsResponse> response = new ArrayList<SimpleProductsAndItemsResponse>();
    doThrow(new ApplicationRuntimeException()).when(
        this.productSearchService).getProductAndItemsByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.productWrapper.getPristineIds());
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRODUCT_WRAPPER)
                .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo( ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductAndItemsByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.productWrapper.getPristineIds());
  }

  @Test
  public void getAllProductCodesAndSkusByPristineIdsExceptionTest() throws Exception {
    List<SimplePristineProductRequestVo> resultVo =
        new ArrayList<>();
    List<SimplePristineProductResponse> response = new ArrayList<SimplePristineProductResponse>();
    doThrow(new RuntimeException()).when(
        this.productSearchService).getProductCodesAndSkusByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, pristineIds);
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRISTINE_IDS)
                .content(this.pristineIdsJson).contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage",equalTo( ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo( ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductCodesAndSkusByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.pristineIds);
  }

  @Test
  public void getAllProductCodesAndSkusByPristineIdsApplicationRuntimeExceptionTest() throws Exception {
    List<SimplePristineProductRequestVo> resultVo =
        new ArrayList<>();
    doThrow(new ApplicationRuntimeException()).when(
        this.productSearchService).getProductCodesAndSkusByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.pristineIds);
    this.mockMvc
        .perform(
            post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_LIST_BY_PRISTINE_IDS)
                .content(this.pristineIdsJson)
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("username", ProductListControllerTest.USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo( ProductErrorCodesEnum.GET_PRODUCT_CODES_AND_PRODUCT_SKUS.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService).getProductCodesAndSkusByPristineIds(
        ProductListControllerTest.STORE_ID, ProductListControllerTest.USERNAME,
        ProductListControllerTest.REQUEST_ID, this.pristineIds);
  }

  @Test
  public void getProductMasterDataDetailByProductCodesAndSkusTest() throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<SimpleProductMasterDataDetailResponse> response =
        new ArrayList<SimpleProductMasterDataDetailResponse>();
    when(this.productSearchService
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus()))
        .thenReturn(resultVo);
    when(this.modelConverter.convertToSimpleProductMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class))).thenReturn(response);
    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST
        + ProductApiPath.PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS)
        .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductListControllerTest.STORE_ID)
        .param("channelId", ProductListControllerTest.CHANNEL_ID)
        .param("clientId", ProductListControllerTest.CLIENT_ID)
        .param("requestId", ProductListControllerTest.REQUEST_ID)
        .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.errorCode", equalTo("")))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus());
    verify(this.modelConverter).convertToSimpleProductMasterDataDetailResponse(
        Mockito.any(MasterDataDetailWithProductAndItemsResponseVo.class));
  }

  @Test
  public void getProductMasterDataDetailByProductCodesAndSkusExceptionTest()
      throws Exception {
    doThrow(new RuntimeException()).when(this.productSearchService)
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus());
    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST
        + ProductApiPath.PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS)
        .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductListControllerTest.STORE_ID)
        .param("channelId", ProductListControllerTest.CHANNEL_ID)
        .param("clientId", ProductListControllerTest.CLIENT_ID)
        .param("requestId", ProductListControllerTest.REQUEST_ID)
        .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(
            jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.INTERNAL_SERVER.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus());
  }

  @Test
  public void getProductMasterDataDetailByProductCodesAndSkusApplicationRuntimeExceptionTest()
      throws Exception {
    MasterDataDetailWithProductAndItemsResponseVo resultVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<SimpleProductMasterDataDetailResponse> response =
        new ArrayList<SimpleProductMasterDataDetailResponse>();
    doThrow(new ApplicationRuntimeException()).when(this.productSearchService)
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus());
    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST
        + ProductApiPath.PRODUCT_MASTER_DATA_DETAIL_BY_PRODUCT_CODES_AND_SKUS)
        .content(this.productWrapperJson).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).param("storeId", ProductListControllerTest.STORE_ID)
        .param("channelId", ProductListControllerTest.CHANNEL_ID)
        .param("clientId", ProductListControllerTest.CLIENT_ID)
        .param("requestId", ProductListControllerTest.REQUEST_ID)
        .param("username", ProductListControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", notNullValue())).andExpect(jsonPath("$.errorCode",
        equalTo(ProductErrorCodesEnum.GET_PRODUCT_MASTER_DATA_DETAIL.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.content", nullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    verify(this.productSearchService)
        .getProductMasterDataDetailByProductCodesAndSkus(ProductListControllerTest.STORE_ID,
            ProductListControllerTest.USERNAME, ProductListControllerTest.REQUEST_ID,
            this.productWrapper.getProductCodes(), this.productWrapper.getProductSkus());
  }

  @Test
  public void testGetProductByMerchantCodeSuccess() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductDetailVo productDetailVo = new ProductDetailVo();
    when(productSearchService.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable))
            .thenReturn(new PageImpl<ProductDetailVo>(Collections.singletonList(productDetailVo)));
    when(modelConverter.toOfficialStoreRequestVO(productDetailRequest))
        .thenReturn(officialStoreRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_CATALOG_BY_MERCHANT_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(productDetailRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(productSearchService).getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable);
    verify(modelConverter)
        .convertToProductDetailResponse(Collections.singletonList(productDetailVo));
    verify(modelConverter).toOfficialStoreRequestVO(productDetailRequest);
  }

  @Test
  public void testGetProductsForMerchantAndCategoryCodesSuccess() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    when(productSearchService.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable))
        .thenReturn(new PageImpl<ActiveProductDetailVo>(Collections.singletonList(activeProductDetailVo)));
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest))
        .thenReturn(activeProductsRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(activeProductRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(productSearchService).getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable);
    verify(modelConverter)
        .convertToActiveProductResponse(Collections.singletonList(activeProductDetailVo));
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductByMerchantCodeThrowSolrException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    doThrow(SolrCustomException.class).when(this.productSearchService)
      .getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO, pageable);
    when(modelConverter.toOfficialStoreRequestVO(productDetailRequest))
        .thenReturn(officialStoreRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_CATALOG_BY_MERCHANT_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(productDetailRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));

    verify(productSearchService).getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable);
    verify(modelConverter).toOfficialStoreRequestVO(productDetailRequest);
  }

  @Test
  public void testGetProductByMerchantCodeThrowApplicationRuntimeException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    doThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION,
      ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())).when(this.productSearchService)
      .getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO, pageable);
    when(modelConverter.toOfficialStoreRequestVO(productDetailRequest))
      .thenReturn(officialStoreRequestVO);

    this.mockMvc
      .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_CATALOG_BY_MERCHANT_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).content(objectMapper.writeValueAsString(productDetailRequest)))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
      pageable);
    verify(modelConverter).toOfficialStoreRequestVO(productDetailRequest);
  }
  @Test
  public void testGetProductsForMerchantAndCategoryCodesThrowSolrException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(productSearchService.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable)).thenThrow(new SolrCustomException(" "));
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest))
        .thenReturn(activeProductsRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(activeProductRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));

    verify(productSearchService).getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductByMerchantCodeThrowException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(productSearchService.getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable)).thenThrow(new Exception());
    when(modelConverter.toOfficialStoreRequestVO(productDetailRequest))
        .thenReturn(officialStoreRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.PRODUCT_CATALOG_BY_MERCHANT_CODE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(productDetailRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage",
            equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getProductsForOfficialStore(STORE_ID, CLIENT_ID, officialStoreRequestVO,
        pageable);
    verify(modelConverter).toOfficialStoreRequestVO(productDetailRequest);
  }

  @Test
  public void testGetProductsForMerchantAndCategoryCodesThrowException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(productSearchService.getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable)).thenThrow(new Exception());
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest))
        .thenReturn(activeProductsRequestVO);

    this.mockMvc
        .perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES)
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
            .param("size", SIZE).content(objectMapper.writeValueAsString(activeProductRequest)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage",
            equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage())))
        .andExpect(
            jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
        pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductsForMerchantAndCategoryCodesThrowApplicxationException()
    throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    doThrow(ApplicationRuntimeException.class).when(productSearchService)
      .getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO, pageable);
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(
      activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST
        + ProductApiPath.GET_ACTIVE_PRODUCTS_BY_MERCHANT_AND_CATEGORY_CODES).contentType(
          MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", PAGE)
        .param("size", SIZE).content(objectMapper.writeValueAsString(activeProductRequest)))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(
        jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getActiveProductsListForMerchant(STORE_ID, activeProductsRequestVO,
      pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductListByStatusSuccess() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    when(productSearchService.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<ActiveProductDetailVo>(Collections.singletonList(activeProductDetailVo)));
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_PRODUCT_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(productSearchService).getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).convertToActiveProductResponse(Collections.singletonList(activeProductDetailVo));
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductListByStatusThrowException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(productSearchService.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable))
        .thenThrow(new Exception());
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_PRODUCT_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetProductListByStatusThrowSolrException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    when(productSearchService.getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable))
      .thenThrow(new SolrCustomException(" "));
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_PRODUCT_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));

    verify(productSearchService).getActiveProductsListForSuspension(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetSuspendedItemListSuccess() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setStatus(STATUS);
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    when(productSearchService.getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable))
        .thenReturn(new PageImpl<ItemInfoVO>(Collections.singletonList(itemInfoVO)));
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_SUSPENDED_ITEM_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    verify(productSearchService).getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
    verify(modelConverter).convertToItemSummaryResponse(Collections.singletonList(itemInfoVO));
  }

  @Test
  public void testGetSuspendedItemlistThrowException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setStatus(STATUS);
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    when(productSearchService.getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable))
        .thenThrow(new Exception());
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_SUSPENDED_ITEM_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getMessage())))
        .andExpect(jsonPath("$.errorCode", equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetSuspendedItemlistThrowSolrException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setStatus(STATUS);
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    doThrow(SolrCustomException.class).when(productSearchService).getSuspendedItemList(STORE_ID,
      activeProductsRequestVO, pageable);
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_SUSPENDED_ITEM_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));

    verify(productSearchService).getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }

  @Test
  public void testGetSuspendedItemlistThrowApplicationException() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setStatus(STATUS);
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    doThrow(ApplicationRuntimeException.class).when(productSearchService).getSuspendedItemList(STORE_ID,
      activeProductsRequestVO, pageable);
    when(modelConverter.toActiveProductsRequestVO(activeProductRequest)).thenReturn(activeProductsRequestVO);

    this.mockMvc.perform(post(ProductApiPath.PRODUCT_LIST + ProductApiPath.GET_SUSPENDED_ITEM_LIST)
        .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("page", PAGE).param("size", SIZE)
        .content(objectMapper.writeValueAsString(activeProductRequest))).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", nullValue()))
      .andExpect(jsonPath("$.errorCode",
        equalTo(ProductErrorCodesEnum.GET_PRODUCT_INFO.getCode())));

    verify(productSearchService).getSuspendedItemList(STORE_ID, activeProductsRequestVO, pageable);
    verify(modelConverter).toActiveProductsRequestVO(activeProductRequest);
  }


  @Test
  public void generateProductScoreByProductSkuTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productSku", ProductListControllerTest.PRODUCT_SKU)
                .param("productCode", ProductListControllerTest.PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, PRODUCT_SKU, PRODUCT_CODE, REQUEST_ID, null,
        false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductCodeNullTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productSku", ProductListControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, PRODUCT_SKU, null, REQUEST_ID, null,
        false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuNullTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null,
        false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductCodeNullExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productService)
      .generateProductScoreByProductSku(Mockito.anyString(), Mockito.anyString(),
          Mockito.isNull(), Mockito.anyString(), Mockito.isNull(), eq(false), Mockito.isNull());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productSku", ProductListControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, PRODUCT_SKU, null, REQUEST_ID, null,
        false, null);
    Mockito.verify(objectMapperMock).writeValueAsString(Mockito.any());
    Mockito.verify(productRetryEventPublishService).insertToRetryPublish(Mockito.any());
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuNullExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productService)
        .generateProductScoreByProductSku(Mockito.anyString(), Mockito.isNull(),
          Mockito.anyString(),Mockito.anyString(), Mockito.isNull(),
          eq(false), Mockito.isNull());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null,
        false, null);
    Mockito.verify(objectMapperMock).writeValueAsString(Mockito.any());
    Mockito.verify(productRetryEventPublishService).insertToRetryPublish(Mockito.any());
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuListNotEmptyTest() throws Exception {
    Mockito.when(
      productService.generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID,
        USERNAME, false, null)).thenReturn(new ArrayList<>());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null,
        false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuProductEmptyTest() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    Mockito.when(
      productService.generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID,
        USERNAME, false, null)).thenReturn(Collections.singletonList(productAndItemsVO));
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null,
        false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuProductNotEmptyTest() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(new Product());
    Mockito.when(productService.generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE,
        REQUEST_ID,null,
        false, null))
        .thenReturn(Collections.singletonList(productAndItemsVO));
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null, false, null);
  }

  @Test
  public void generateProductScoreByProductSkuProductSkuNullRetryExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productService)
      .generateProductScoreByProductSku(Mockito.anyString(), Mockito.isNull(),
        Mockito.anyString(), Mockito.anyString(), Mockito.isNull(), eq(false), Mockito.isNull());
    Mockito.doThrow(RuntimeException.class).when(objectMapperMock).writeValueAsString(Mockito.any());
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productCode", PRODUCT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(productService)
      .generateProductScoreByProductSku(STORE_ID, null, PRODUCT_CODE, REQUEST_ID, null, false, null);
    Mockito.verify(objectMapperMock).writeValueAsString(Mockito.any());
  }

  @Test
  public void generateProductScoreByProductSkuFalseTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.GENERATE_PRODUCT_SCORE_BY_PRODUCT_SKU_OR_PRODUCT_CODE)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
  }

  @Test
  public void updateMasterDataFieldsInProductTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.UPDATE_MASTER_DATA_FIELDS_IN_PRODUCT)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)
                .param("productSku", ProductListControllerTest.PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
    Mockito.verify(productService).updateMasterDataFieldsInProduct(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void updateMasterDataFieldsInProductFalseTest() throws Exception {
    this.mockMvc
        .perform(
            get(ProductApiPath.PRODUCT_LIST + ProductApiPath.UPDATE_MASTER_DATA_FIELDS_IN_PRODUCT)
                .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", ProductListControllerTest.STORE_ID)
                .param("channelId", ProductListControllerTest.CHANNEL_ID)
                .param("clientId", ProductListControllerTest.CLIENT_ID)
                .param("requestId", ProductListControllerTest.REQUEST_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(ProductListControllerTest.REQUEST_ID)));
  }


  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productSearchService);
    verifyNoMoreInteractions(this.objectMapperMock);
    verifyNoMoreInteractions(this.productRetryEventPublishService);
  }

}
