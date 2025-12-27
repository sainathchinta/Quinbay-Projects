package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import com.gda.mta.product.dto.EditProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeAndKeywordListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;

public class ProductRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final VerificationMode AT_LEAST_TWO = Mockito.times(2);
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final VerificationMode AT_LEAST_NONE = Mockito.times(0);
  private static final Pageable DEFAULT_PAGEABLE =
      PageRequest.of(0, 10, Sort.by(Direction.ASC, "id"));
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_CATEGORY_CODE = "CAT-10001";
  private static final String DEFAULT_BP_CODE = "BP_CODE";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_PRODUCT_CODE_2 = "MTA-0000002";
  private static final String DEFAULT_PRODUCT_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_PRODUCT_ID_2 = UUID.randomUUID().toString();
  private static final String DEFAULT_USER_NAME = "USER";
  private static final String REQUEST_ID = "requestId";
  private static final String USER_NAME = "username";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String BRAND = "brand";
  private static final Double LENGTH = 1.0;
  private static final Double WIDTH = 1.0;
  private static final Double HEIGHT = 1.0;
  private static final Double WEIGHT = 1.0;
  private static final Double SHIPPING_WEIGHT = 0.1;
  private static final Integer DANGEROUS_GOODS_LEVEL = 1;
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String ERROR_CODE = "errorCode";
  private static final String CATALOG_CODE = "CatalogCode";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";

  private static final String GENERATED_ITEM_NAME_1 = "Item Red S";
  private static final String GENERATED_ITEM_NAME_2 = "Item Blue S";
  private static final String GENERATED_ITEM_NAME_3 = "Item Red L";
  private static final String GENERATED_ITEM_NAME_4 = "Item Blue L";

  private static final String ATTRIBUTE_CODE_1 = "ATT-123";
  private static final String ATTRIBUTE_CODE_2 = "ATT-456";

  private static final String ATTRIBUTE_VALUE_1 = "Red";
  private static final String ATTRIBUTE_VALUE_2 = "Blue";
  private static final String ATTRIBUTE_VALUE_3 = "L";
  private static final String ATTRIBUTE_VALUE_4 = "S";


  @InjectMocks
  private ProductRepositoryBean productRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private ProductOutbound productOutbound;

  @Captor
  private ArgumentCaptor<SimpleMasterProductUpdateRequest> simpleMasterProductUpdateRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemImageRequest> productAndItemImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductRequest> productRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCodesRequest> productCodesRequestArgumentCaptor;

  private String requestId;
  private SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO;
  private List<ConfigurationStatusRequest> configurationStatusRequestList;
  private List<ConfigurationStatusResponse> configurationStatusResponses;


  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(productOutbound);
    MDC.clear();
  }

  private Product getProduct() {
    Product product =
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes())
            .brand("Brand 1").storeId(DEFAULT_STORE_ID).promoSKU(false).build();
    product.setId(UUID.randomUUID().toString());
    product.getProductItems().add(
        new ProductItem(product, "1000000000001", null, product.getName() + " - S", null,
            DEFAULT_STORE_ID));
    product.getProductItems().add(
        new ProductItem(product, "1000000000002", null, product.getName() + " - M", null,
            DEFAULT_STORE_ID));
    product.getProductItems().add(
        new ProductItem(product, "1000000000003", null, product.getName() + " - L", null,
            DEFAULT_STORE_ID));
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setId(UUID.randomUUID().toString());
    }
    product.getProductAttributes().add(new ProductAttribute(
        new Attribute(null, AttributeType.DESCRIPTIVE_ATTRIBUTE, false, DEFAULT_STORE_ID), product,
        "Ukuran Pakaian", false, 1, DEFAULT_STORE_ID));
    product.getProductCategories()
        .add(new ProductCategory(product, new Category(), DEFAULT_STORE_ID));
    return product;
  }

  private ProductItemLevel3 getNewlyAddedItem() {
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    TreeMap<String, String> attributeCodeToValueMap = new TreeMap<>();
    attributeCodeToValueMap.put(DEFAULT_STORE_ID, "Red");
    attributeCodeToValueMap.put(DEFAULT_BP_CODE, "S");
    productItemLevel3.setItemAttributesMap(attributeCodeToValueMap);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setAttributeCode("FA-00001");
    productLevel3Attribute.setValues(Collections.singletonList("Mearh"));
    productItemLevel3.getItemAttributes().add(productLevel3Attribute);
    ProductLevel3Attribute productLevel3Attribute1 = new ProductLevel3Attribute();
    productLevel3Attribute1.setAttributeCode("ATT-0001");
    productLevel3Attribute1.setValues(Collections.singletonList("ROYAL"));
    productItemLevel3.getItemAttributes().add(productLevel3Attribute1);
    return productItemLevel3;
  }

  private void setMdcParameters(String storeId, String channelId, String clientId, String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
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
    return productDetailResponse;
  }

  public ProductRepositoryBean getProductRepositoryBean() {
    return this.productRepositoryBean;
  }

  private List<Product> getProducts() {
    List<Product> products = new ArrayList<Product>();

    products.add(
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build());
    products.add(
        new Product.Builder().productCode(null).name("Produk 2").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 2".getBytes()).build());
    products.add(
        new Product.Builder().productCode(null).name("Produk 3").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 3".getBytes()).build());
    return products;
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GdnBaseLookup.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    setRequestId(UUID.randomUUID().toString());
    GdnBaseRestResponse baseResponse =
        new GdnBaseRestResponse(ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    GdnBaseRestResponse baseResponseError =
        new GdnBaseRestResponse("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<ProductDetailResponse> productDetailResponse =
        new GdnRestSingleResponse<ProductDetailResponse>(new ProductDetailResponse(),
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<ProductDetailResponse> productDetailResponseError =
        new GdnRestSingleResponse<ProductDetailResponse>("Read Timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, null,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    Mockito.when(this.pcbFeign.updateProductViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE),
        Mockito.anyBoolean())).thenReturn(baseResponse);
    Mockito.when(this.pcbFeign.updateProductViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2),
        Mockito.anyBoolean())).thenReturn(baseResponseError);
    Mockito.when(
            this.pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseResponse);
    Mockito.when(this.pcbFeign.updateProductActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE),
        Mockito.anyBoolean())).thenReturn(baseResponse);
    Mockito.when(this.pcbFeign.updateProductActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2),
        Mockito.anyBoolean())).thenReturn(baseResponseError);
    Mockito.when(
            this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.any(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID), eq(true)))
        .thenReturn(productDetailResponse);
    Mockito.when(
            this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.any(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID_2), eq(true)))
        .thenReturn(productDetailResponseError);
    Mockito.when(this.pcbFeign.updateProductContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any())).thenReturn(baseResponse);
    Mockito.when(this.pcbFeign.updateProductImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(baseResponse);

    simpleMasterProductUpdateRequestDTO = new SimpleMasterProductUpdateRequestDTO();
    simpleMasterProductUpdateRequestDTO.setProductCode(PRODUCT_CODE);
    simpleMasterProductUpdateRequestDTO.setName(PRODUCT_NAME);
    simpleMasterProductUpdateRequestDTO.setBrand(BRAND);
    simpleMasterProductUpdateRequestDTO.setLength(LENGTH);
    simpleMasterProductUpdateRequestDTO.setWidth(WIDTH);
    simpleMasterProductUpdateRequestDTO.setHeight(HEIGHT);
    simpleMasterProductUpdateRequestDTO.setWeight(WEIGHT);
    simpleMasterProductUpdateRequestDTO.setShippingWeight(SHIPPING_WEIGHT);
    simpleMasterProductUpdateRequestDTO.setDangerousGoodsLevel(DANGEROUS_GOODS_LEVEL);
    ConfigurationStatusRequest configurationStatusRequest = new ConfigurationStatusRequest();
    configurationStatusRequest.setBusinessPartnerCode(DEFAULT_BP_CODE);
    configurationStatusRequest.setCategoryCode(DEFAULT_CATEGORY_CODE);
    configurationStatusRequestList = Arrays.asList(configurationStatusRequest);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", false);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteItemsFromPcbForMissingVariants", false);
  }

  public void setProductRepositoryBean(ProductRepositoryBean productRepositoryBean) {
    this.productRepositoryBean = productRepositoryBean;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void testActivate() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    Mockito.when(
        pcbFeign.activateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    getProductRepositoryBean().activate(getProduct());
    Mockito.verify(pcbFeign)
        .activateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testActivateException() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    Mockito.when(
        pcbFeign.activateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductRepositoryBean().activate(getProduct());
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(pcbFeign)
        .activateProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testFindProductItemsByProductId() throws Exception {
    GdnRestListResponse<ProductItemResponse> response = new GdnRestListResponse<>(null, null, true, "");
    Mockito.when(
        pcbFeign.getProductItemWithAttributeValues(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    getProductRepositoryBean().findProductItemsByProductId(PRODUCT_ID);
    Mockito.verify(pcbFeign)
        .getProductItemWithAttributeValues(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void testFindByStoreIdAndProductCodeExactMatch() throws Exception {
    GdnRestListResponse<ProductResponse> response = new GdnRestListResponse<>(null, null, true, "");
    Mockito.when(
            pcbFeign.getProductByProductCodeExactMatch(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyString()))
        .thenReturn(response);
    getProductRepositoryBean().findByStoreIdAndProductCodeExactMatch(DEFAULT_STORE_ID, PRODUCT_CODE, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign)
        .getProductByProductCodeExactMatch(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyString());
  }

  @Test
  public void testGetProductCountByViewable() throws Exception {
    GdnRestSingleResponse<SingleObjectResponse<Long>> response =
        new GdnRestSingleResponse<>(null, null, true, null, "");
    Mockito.when(pcbFeign.getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    getProductRepositoryBean().getProductCountByViewable(true);
    Mockito.verify(pcbFeign)
        .getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testGetProductCountByViewableWhenProductCountNull() throws Exception {
    GdnRestSingleResponse<SingleObjectResponse<Long>> response =
        new GdnRestSingleResponse(null, null, true, new SingleObjectResponse<>(), "");
    Mockito.when(pcbFeign.getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    getProductRepositoryBean().getProductCountByViewable(true);
    Mockito.verify(pcbFeign)
        .getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testGetProductCountByViewableWhenProductCountNotNull() throws Exception {
    GdnRestSingleResponse<SingleObjectResponse<Long>> response =
        new GdnRestSingleResponse(null, null, true, new SingleObjectResponse<>((long)2), "");
    Mockito.when(pcbFeign.getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    getProductRepositoryBean().getProductCountByViewable(true);
    Mockito.verify(pcbFeign)
        .getProductCountByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testFilterCategoryHierarchyByCategoryCode() throws Exception {
    GdnRestListResponse<CategoryResponse> response = new GdnRestListResponse<>(null, null, true, "");
    Mockito.when(
        pcbFeign.filterCategoryHierarchyByCategoryCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    getProductRepositoryBean().filterCategoryHierarchyByCategoryCode(REQUEST_ID, USER_NAME, DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign)
        .filterCategoryHierarchyByCategoryCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void testDelete() throws Exception {
    ProductDetailResponse product = getProductDetailResponse();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    Mockito.when(
        pcbFeign.discardProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(ProductRequest.class))).thenReturn(response);
    getProductRepositoryBean().delete(product);
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .discardProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(ProductRequest.class));
  }

  @Test
  public void testDeleteWithFalseResponse() throws Exception {
    ProductDetailResponse product = getProductDetailResponse();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, getRequestId());
    Mockito.when(
        pcbFeign.discardProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(ProductRequest.class))).thenReturn(response);
    try {
      getProductRepositoryBean().delete(product);
    } catch (ApplicationException e) {
      Mockito.verify(pcbFeign, AT_LEAST_ONE)
          .discardProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.any(ProductRequest.class));
    }
  }

  @Test
  public void testFindByStoreId() throws Exception {
    List<Product> products = getProducts();
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(
        pcbFeign.getProductSummary(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(response);
    Page<Product> result = getProductRepositoryBean().findByStoreId(DEFAULT_STORE_ID, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductSummary(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());
    Assertions.assertEquals(3, result.getTotalElements());
  }

  @Test
  public void testFindByStoreIdAndName() throws Exception {
    List<Product> products = new ArrayList<Product>();
    products.add(
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build());
    products.add(
        new Product.Builder().productCode(null).name("Produk 2").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 2".getBytes()).build());
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(
        pcbFeign.getProductByName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq("Produk 1"))).thenReturn(response);
    Page<Product> result = getProductRepositoryBean()
        .findByStoreIdAndName(DEFAULT_STORE_ID, "Produk 1", DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductByName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq("Produk 1"));
    Assertions.assertEquals(2, result.getTotalElements());
  }

  @Test
  public void testFindByStoreIdAndNameAndViewableAndActivated() throws Exception {
    String productCode = UUID.randomUUID().toString();
    List<Product> products = new ArrayList<Product>();
    Product product =
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build();
    product.setViewable(true);
    product.setActivated(true);
    products.add(product);
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product2 : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product2, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(
        pcbFeign.getProductByNameAndViewableAndActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq("Produk 1"), eq(true),
            eq(true))).thenReturn(response);
    Page<Product> result = getProductRepositoryBean()
        .findByStoreIdAndNameAndViewableAndActivated(DEFAULT_STORE_ID, "Produk 1", true, true,
            DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign)
        .getProductByNameAndViewableAndActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq("Produk 1"), eq(true),
            eq(true));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test
  public void testFindByStoreIdAndProductCode() throws Exception {
    String productCode = UUID.randomUUID().toString();
    List<Product> products = new ArrayList<Product>();
    products.add(
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build());
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(pcbFeign.getProductByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(productCode)))
        .thenReturn(response);
    Page<Product> result =
        getProductRepositoryBean().findByStoreIdAndProductCode(DEFAULT_STORE_ID, productCode, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(productCode));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test
  public void testFindByStoreIdAndViewableAndActivated() throws Exception {
    String productCode = UUID.randomUUID().toString();
    List<Product> products = new ArrayList<Product>();
    Product product =
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build();
    product.setViewable(true);
    product.setActivated(true);
    products.add(product);
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product2 : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product2, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(
            pcbFeign.getProductByViewableAndActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(true), eq(true)))
        .thenReturn(response);
    Page<Product> result =
        getProductRepositoryBean().findByStoreIdAndViewableAndActivated(DEFAULT_STORE_ID, true, true, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign)
        .getProductByViewableAndActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(true), eq(true));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test
  public void testFindByViewable() throws Exception {
    String productCode = UUID.randomUUID().toString();
    List<Product> products = new ArrayList<Product>();
    Product product =
        new Product.Builder().productCode(null).name("Produk 1").length(1.0).width(1.0).height(1.0)
            .weight(1.0).shippingWeight(1.0).description("Deskripsi Produk 1".getBytes()).build();
    product.setViewable(true);
    products.add(product);
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product2 : products) {
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(product2, productResponse);
      productResponses.add(productResponse);
    }
    GdnRestListResponse<ProductResponse> response =
        new GdnRestListResponse<ProductResponse>(null, null, true, productResponses,
            new PageMetaData(DEFAULT_PAGEABLE.getPageSize(), DEFAULT_PAGEABLE.getPageNumber(),
                productResponses.size()), getRequestId());
    Mockito.when(pcbFeign.getProductByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(true))).thenReturn(response);
    Page<Product> result =
        getProductRepositoryBean().findByStoreIdAndViewable(DEFAULT_STORE_ID, true, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductByViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(true));
    Assertions.assertEquals(1, result.getTotalElements());
  }

  @Test
  public void testFindOne() throws Exception {
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productItemResponse.setId(UUID.randomUUID().toString());
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse
          .setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      ProductAttributeValueResponse valueResponse = new ProductAttributeValueResponse();
      valueResponse.setMarkForDelete(true);
      productAttributeResponse.setProductAttributeValues(Arrays.asList(valueResponse));
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse,
            getRequestId());
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(product.getId()), eq(true))).thenReturn(response);
    Product result = getProductRepositoryBean().findOne(product.getId());
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(product.getId()), eq(true));
    Assertions.assertEquals("Produk 1", result.getName());
    Assertions.assertEquals(3, result.getProductItems().size());
    Assertions.assertEquals(1, result.getProductAttributes().size());
    Assertions.assertEquals(1, result.getProductCategories().size());
  }

  @Test
  public void testFindOneWithItemAttributeValues() throws Exception {
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productItemResponse.setId(UUID.randomUUID().toString());
      productDetailResponse.getProductItemResponses().add(productItemResponse);
      ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
      productItemAttributeValue.setValue("Value");
      AttributeResponse attribute = new AttributeResponse();
      attribute.setName("Family Colour");
      productItemAttributeValue.setAttributeResponse(attribute);
      productItemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValue));
      Image image = new Image();
      image.setId("ID");
      image.setLocationPath("Location path");
      productItemResponse.setImages(Arrays.asList(image));
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse
          .setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      ProductAttributeValueResponse valueResponse = new ProductAttributeValueResponse();
      valueResponse.setMarkForDelete(true);
      productAttributeResponse.setProductAttributeValues(Arrays.asList(valueResponse));
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse,
            getRequestId());
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(product.getId()), eq(true))).thenReturn(response);
    Product result = getProductRepositoryBean().findOne(product.getId());
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(product.getId()), eq(true));
    Assertions.assertEquals("Produk 1", result.getName());
    Assertions.assertEquals(3, result.getProductItems().size());
    Assertions.assertEquals(1, result.getProductAttributes().size());
    Assertions.assertEquals(1, result.getProductCategories().size());
    Assertions.assertTrue(result.getProductAttributes().get(0).getProductAttributeValues().get(0).isMarkForDelete(),
        "markForDelete must be true");
  }

  @Test
  public void testSave() throws Exception {
    Product product = getProduct();
    product.setId(null);
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    request.setCreatedMerchant("INTERNAL");
    Mockito.when(
        pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(request))).thenReturn(response);
    getProductRepositoryBean().save(product, "INTERNAL");
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(request));
  }

  @Test
  public void testSaveWithInvalidGdnBaseRestResponse() throws Exception {
    Product product = getProduct();
    product.setId(null);
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    request.setCreatedMerchant("INTERNAL");
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, getRequestId());
    Mockito.when(
        pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(request))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductRepositoryBean().save(product, "INTERNAL");
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(request));
  }

  @Test
  public void testUpdate() throws Exception {
    long l = 10;
    getRequests result = getGetRequests(l);
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, result.productDetailResponse,
            getRequestId());
    Mockito.when(
      pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture())).thenReturn(response);
    Mockito.when(pcbFeign
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true)))
        .thenReturn(response2);
    Mockito.when(pcbFeign
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(response);
    getProductRepositoryBean().update(result.product, false, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getPristineCategory());
  }

  @Test
  public void testUpdateSkipSaveItem() throws Exception {
    long l = 10;
    getRequests result = getGetRequests(l);
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, result.productDetailResponse,
            getRequestId());
    Mockito.when(
        pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture())).thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    getProductRepositoryBean().update(result.product, false, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(), true, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getPristineCategory());
  }

  private getRequests getGetRequests(long l) {
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse
          .setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    getRequests result = new getRequests(product, productDetailResponse);
    return result;
  }

  private static class getRequests {
    public final Product product;
    public final ProductDetailResponse productDetailResponse;

    public getRequests(Product product, ProductDetailResponse productDetailResponse) {
      this.product = product;
      this.productDetailResponse = productDetailResponse;
    }
  }

  @Test
  public void testUpdateAddDeleteVariantSwitch() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(productRepositoryBean, "familyColorAttributeCode", "FA-00001");
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    ProductImage productImage1 = new ProductImage();
    productImage1.setCommonImage(true);
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().addAll(Arrays.asList(productImage2, productImage1, productImage3));
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap = new HashMap<>();
    productItemAttributeValueRequestMap.put("Red10001", new ProductItemAttributeValueRequest());
    productItemAttributeValueRequestMap.put("SBPC_CODE", new ProductItemAttributeValueRequest());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture()))
        .thenReturn(response);
    AttributeResponse attributeResponse = new AttributeResponse();
    GdnRestListResponse<AttributeResponse> response1 =
        new GdnRestListResponse<>(Collections.singletonList(attributeResponse), null, REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001"))
        .thenReturn(response1);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    getProductRepositoryBean().update(product, false, false, true, false,
        Arrays.asList(getNewlyAddedItem(), getNewlyAddedItem()), productItemAttributeValueRequestMap,
        Collections.singletonList(product.getProductItems().get(0).getSkuCode()), false, false, new ArrayList<>(),
        new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001");
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getPristineCategory());
    Assertions.assertEquals(2,productRequestArgumentCaptor.getValue().getNewlyAddedProductItems().size());
    Assertions.assertEquals(2,
        productRequestArgumentCaptor.getValue().getNewlyAddedProductItems().get(0).getAttributesMap().size());
  }

  @Test
  public void testUpdateAddDeleteVariantSwitchCombinedEdit() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(productRepositoryBean, "familyColorAttributeCode", "FA-00001");
    ReflectionTestUtils.setField(productRepositoryBean, "combinedEditFlowEnabled", true);

    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    ProductImage productImage1 = new ProductImage();
    productImage1.setCommonImage(true);
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().addAll(Arrays.asList(productImage2, productImage1, productImage3));
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap = new HashMap<>();
    productItemAttributeValueRequestMap.put("Red", new ProductItemAttributeValueRequest());
    productItemAttributeValueRequestMap.put("S", new ProductItemAttributeValueRequest());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
      new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
      new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
        pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture()))
      .thenReturn(response);
    AttributeResponse attributeResponse = new AttributeResponse();
    GdnRestListResponse<AttributeResponse> response1 =
      new GdnRestListResponse<>(Collections.singletonList(attributeResponse), null, REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001"))
      .thenReturn(response1);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    getProductRepositoryBean().update(product, false, false, true, false,
      Arrays.asList(getNewlyAddedItem(), getNewlyAddedItem()), productItemAttributeValueRequestMap,
      Collections.singletonList(product.getProductItems().get(0).getSkuCode()), false, false, new ArrayList<>(),
        new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, Mockito.times(3))
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001");
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getPristineCategory());
    Assertions.assertEquals(2,productRequestArgumentCaptor.getValue().getNewlyAddedProductItems().size());
  }

  @Test
  public void testUpdateAddDeleteFalseSwitchCombinedEdit() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "addDeleteVariantSwitch", false);
    ReflectionTestUtils.setField(productRepositoryBean, "familyColorAttributeCode", "FA-00001");
    ReflectionTestUtils.setField(productRepositoryBean, "combinedEditFlowEnabled", true);

    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    ProductImage productImage1 = new ProductImage();
    productImage1.setCommonImage(true);
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().addAll(Arrays.asList(productImage2, productImage1, productImage3));
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap = new HashMap<>();
    productItemAttributeValueRequestMap.put("Red", new ProductItemAttributeValueRequest());
    productItemAttributeValueRequestMap.put("S", new ProductItemAttributeValueRequest());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
      new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
      new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
        pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture()))
      .thenReturn(response);
    AttributeResponse attributeResponse = new AttributeResponse();
    GdnRestListResponse<AttributeResponse> response1 =
      new GdnRestListResponse<>(Collections.singletonList(attributeResponse), null, REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001"))
      .thenReturn(response1);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    getProductRepositoryBean().update(product, false, false, true, false,
      Arrays.asList(getNewlyAddedItem(), getNewlyAddedItem()), productItemAttributeValueRequestMap,
      Collections.singletonList(product.getProductItems().get(0).getSkuCode()), false, false, new ArrayList<>(),
        new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, Mockito.times(2))
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001");
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertFalse(productRequestArgumentCaptor.getValue().getPristineCategory());
    Assertions.assertEquals(2,productRequestArgumentCaptor.getValue().getNewlyAddedProductItems().size());
  }

  @Test
  public void testUpdateAddDeleteVariantSwitchExceptionTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(productRepositoryBean, "familyColorAttributeCode", "FA-00001");
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    ProductImage productImage1 = new ProductImage();
    productImage1.setCommonImage(true);
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().addAll(Arrays.asList(productImage2, productImage1, productImage3));
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap = new HashMap<>();
    productItemAttributeValueRequestMap.put("Red", new ProductItemAttributeValueRequest());
    productItemAttributeValueRequestMap.put("S", new ProductItemAttributeValueRequest());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture()))
        .thenReturn(response);
    AttributeResponse attributeResponse = new AttributeResponse();
    GdnRestListResponse<AttributeResponse> response1 =
        new GdnRestListResponse<>(Collections.singletonList(attributeResponse), null, REQUEST_ID);
    response1.setSuccess(false);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001"))
        .thenReturn(response1);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        getProductRepositoryBean().update(product, false, false, true, false,
            Arrays.asList(getNewlyAddedItem(), getNewlyAddedItem()), productItemAttributeValueRequestMap, new ArrayList<>(),
            false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
      });
    }finally {
      Mockito.verify(pcbFeign, AT_LEAST_ONE)
          .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
      Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001");
    }
  }

  @Test
  public void testUpdateAddDeleteVariantSwitchException1Test() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(productRepositoryBean, "familyColorAttributeCode", "FA-00001");
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    ProductImage productImage1 = new ProductImage();
    productImage1.setCommonImage(true);
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    productImage2.setMarkForDelete(true);
    product.getProductImages().addAll(Arrays.asList(productImage2, productImage1, productImage3));
    product.getProductItems().get(0).getProductItemImages().add(new ProductItemImage());
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap = new HashMap<>();
    productItemAttributeValueRequestMap.put("Red", new ProductItemAttributeValueRequest());
    productItemAttributeValueRequestMap.put("S", new ProductItemAttributeValueRequest());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response2 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture()))
        .thenReturn(response);
    GdnRestListResponse<AttributeResponse> response1 =
        new GdnRestListResponse<>(new ArrayList<>(), null, REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001"))
        .thenReturn(response1);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response2);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        getProductRepositoryBean().update(product, false, false, true, false,
            Arrays.asList(getNewlyAddedItem(), getNewlyAddedItem()), productItemAttributeValueRequestMap,
            new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(),
            EditProductResponse.builder().build());
      });
    }finally {
      Mockito.verify(pcbFeign, AT_LEAST_ONE)
          .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
      Mockito.verify(pcbFeign).getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, "FA-00001");
    }
  }

  @Test
  public void testUpdateWithInvalidGdnBaseRestResponse() throws Exception {
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, false, null, new PageMetaData(), getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse,
            getRequestId());
    Mockito.when(
      pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture())).thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductRepositoryBean().update(product, null, false, true, false, new ArrayList<>(), new HashedMap(),
            new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(),
            EditProductResponse.builder().build());
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        Assertions.assertNull(productRequestArgumentCaptor.getValue().getPristineCategory());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign, AT_LEAST_NONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(request));
  }

  @Test
  public void testUpdateWithInvalidGdnBaseRestResponseProductItem() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse
          .setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse,
            getRequestId());
    Mockito.when(
      pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class))).thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValue));
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response2);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
            new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        Assertions.assertTrue(productRequestArgumentCaptor.getValue().getPristineCategory());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testUpdateWithInvalidGdnBaseRestResponseProductItemAttributeDeletedTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    long l = 10;
    Product product = getProduct();
    product.getProductItems().forEach(productItem -> productItem.setSkuCode(""));
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValue));
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response2);
    ApplicationException applicationException = null;
    try {
      getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
          new ArrayList<>(), false, false, Arrays.asList(DEFAULT_PRODUCT_CODE), new ProductL3Response(), EditProductResponse.builder().build());
    } catch (Exception e) {
      applicationException = (ApplicationException) e;
      Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
    } finally {
      Mockito.verify(pcbFeign)
          .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
      Mockito.verify(pcbFeign, AT_LEAST_TWO)
          .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
      Mockito.verify(pcbFeign, AT_LEAST_ONE)
          .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.any());
      Assertions.assertTrue(productRequestArgumentCaptor.getValue().getPristineCategory());
    }
  }


  @Test
  public void testUpdateWithInvalidGdnBaseRestResponseProductItemDeletedTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(List.of(productItemAttributeValue));
    response3.getValue().getProductItemResponses().iterator().next().setSkuCode(PRODUCT_CODE);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response2);
    ApplicationException applicationException = null;
    try {
      getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
          Arrays.asList(PRODUCT_CODE), false, false, Arrays.asList(DEFAULT_PRODUCT_CODE), new ProductL3Response(), EditProductResponse.builder().build());
    } catch (Exception e) {
      applicationException = (ApplicationException) e;
      Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
    } finally {
      Mockito.verify(pcbFeign)
          .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), productRequestArgumentCaptor.capture());
      Mockito.verify(pcbFeign, AT_LEAST_TWO)
          .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
      Mockito.verify(pcbFeign, AT_LEAST_ONE)
          .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.any());
      Assertions.assertTrue(productRequestArgumentCaptor.getValue().getPristineCategory());
    }
  }

  @Test
  public void missingVariantsDeletionFromPcbTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteItemsFromPcbForMissingVariants", true);
    long l = 10;
    ProductL3Response productL3Response = new ProductL3Response();
    Map<String, String> itemCodeAndSku = new HashMap<>();
    itemCodeAndSku.put("ItemSku", "itemCodeMissing");
    productL3Response.setItemSkuItemCodeMap(itemCodeAndSku);
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(List.of(productItemAttributeValue));
    response3.getValue().getProductItemResponses().iterator().next().setSkuCode(PRODUCT_CODE);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response3);
    getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(Collections.singletonList(PRODUCT_CODE)), false, false,
        new ArrayList<>(Collections.singletonList(DEFAULT_PRODUCT_CODE)), productL3Response, EditProductResponse.builder().build());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class));
    Mockito.verify(pcbFeign)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void missingVariantsDeletionFromPcbFalseTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteItemsFromPcbForMissingVariants", true);
    long l = 10;
    ProductL3Response productL3Response = new ProductL3Response();
    Map<String, String> itemCodeAndSku = new HashMap<>();
    itemCodeAndSku.put("productCode", "productCode");
    productL3Response.setItemSkuItemCodeMap(itemCodeAndSku);
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(List.of(productItemAttributeValue));
    response3.getValue().getProductItemResponses().iterator().next().setSkuCode(PRODUCT_CODE);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response3);
    getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(Collections.singletonList(PRODUCT_CODE)), false, false,
        new ArrayList<>(Collections.singletonList(DEFAULT_PRODUCT_CODE)), productL3Response, EditProductResponse.builder().build());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class));
    Mockito.verify(pcbFeign)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void missingVariantsDeletionFromPcbFalseNullTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteItemsFromPcbForMissingVariants", true);
    long l = 10;
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setItemSkuItemCodeMap(null);
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(List.of(productItemAttributeValue));
    response3.getValue().getProductItemResponses().iterator().next().setSkuCode(PRODUCT_CODE);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response3);
    getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(Collections.singletonList(PRODUCT_CODE)), false, false,
        new ArrayList<>(Collections.singletonList(DEFAULT_PRODUCT_CODE)), productL3Response, EditProductResponse.builder().build());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class));
    Mockito.verify(pcbFeign)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void missingVariantsDeletionFromPcbFalseXproductResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "deleteAllItemsForDeletedAttributeEnabled", true);
    ReflectionTestUtils.setField(productRepositoryBean, "deleteItemsFromPcbForMissingVariants", true);
    long l = 10;
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setItemSkuItemCodeMap(null);
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion());
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemResponse productItemResponse = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, productItemResponse);
      productDetailResponse.getProductItemResponses().add(productItemResponse);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(productAttribute, productAttributeResponse);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
      attributeResponse.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
      productAttributeResponse.setAttribute(attributeResponse);
      productDetailResponse.getProductAttributeResponses().add(productAttributeResponse);
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(productCategory, productCategoryResponse);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
      productCategoryResponse.setCategory(categoryResponse);
      productDetailResponse.getProductCategoryResponses().add(productCategoryResponse);
    }
    GdnRestListResponse<NewlySavedItemResponse> response =
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(), getRequestId());
    GdnBaseRestResponse response2 = new GdnBaseRestResponse(null, null, false, getRequestId());
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse, getRequestId());
    Mockito.when(
            pcbFeign.updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true))).thenReturn(response3);
    ProductItemAttributeValueResponse productItemAttributeValue = new ProductItemAttributeValueResponse();
    productItemAttributeValue.setValue("Value");
    AttributeResponse attribute = new AttributeResponse();
    attribute.setName("Family Colour");
    attribute.setAttributeCode(DEFAULT_PRODUCT_CODE);
    productItemAttributeValue.setAttributeResponse(attribute);
    response3.getValue().getProductItemResponses().iterator().next()
        .setProductItemAttributeValueResponses(List.of(productItemAttributeValue));
    response3.getValue().getProductItemResponses().iterator().next().setSkuCode(PRODUCT_CODE);
    Mockito.when(
        pcbFeign.saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any())).thenReturn(response3);
    getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
        new ArrayList<>(Collections.singletonList(PRODUCT_CODE)), false, false,
        new ArrayList<>(Collections.singletonList(DEFAULT_PRODUCT_CODE)), null, EditProductResponse.builder().build());
    Mockito.verify(pcbFeign, AT_LEAST_TWO)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(true), eq(false), eq(false), eq(false), Mockito.any(ProductRequest.class));
    Mockito.verify(pcbFeign)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testUpdateWithInvalidVersion() throws Exception {
    long l = 10;
    Product product = getProduct();
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils
        .copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(l);
    product.setVersion(productDetailResponse.getVersion() - 1);
    GdnRestSingleResponse<ProductDetailResponse> response3 =
        new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, productDetailResponse,
            getRequestId());
    Mockito.when(pcbFeign
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true)))
        .thenReturn(response3);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        getProductRepositoryBean().update(product, true, false, true, false, new ArrayList<>(), new HashedMap(),
            new ArrayList<>(), false, false, new ArrayList<>(), new ProductL3Response(),
            EditProductResponse.builder().build());
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(pcbFeign, AT_LEAST_NONE)
        .updateProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(),
            Mockito.anyBoolean(), Mockito.any());
    Mockito.verify(pcbFeign, AT_LEAST_ONE)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), eq(true));
    Mockito.verify(pcbFeign, AT_LEAST_NONE)
        .saveProductItem(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateViewableTest() throws Exception {
    this.productRepositoryBean.updateViewable(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE, true);
    Mockito.verify(this.pcbFeign)
        .updateProductViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE), Mockito.anyBoolean());
  }

  @Test
  public void updateViewableWithErrorTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean
            .updateViewable(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2, true);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .updateProductViewable(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2), Mockito.anyBoolean());
  }


  @Test
  public void updateProductImageNameTest() throws Exception {
    ActivateImageRequest request = new ActivateImageRequest();
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.updateProductImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    this.productRepositoryBean.updateProductImageName(request);
    Mockito.verify(pcbFeign, Mockito.times(1))
        .updateProductImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateProductImageNameWithErrorTest() throws Exception {
    ActivateImageRequest request = new ActivateImageRequest();
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, false, value, getRequestId());
    Mockito.when(pcbFeign.updateProductImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.updateProductImageName(request);
      });
    } finally {
      Mockito.verify(pcbFeign, Mockito.times(1))
          .updateProductImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateProductImageNameWithProductActivateImageRequestTest() throws Exception {
    ProductActivateImageRequest request = new ProductActivateImageRequest();
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.updateProductImagesName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any())).thenReturn(response);
    this.productRepositoryBean.updateProductImagesName(request);
    Mockito.verify(pcbFeign)
        .updateProductImagesName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void updateProductImageNameWithProductActivateImageRequestErrorTest() throws Exception {
    ProductActivateImageRequest request = new ProductActivateImageRequest();
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, false, value, getRequestId());
    Mockito.when(pcbFeign.updateProductImagesName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        this.productRepositoryBean.updateProductImagesName(request);
      });
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.DATA_ACCESS, e.getErrorCodes());
      throw e;
    }
    Mockito.verify(pcbFeign)
        .updateProductImagesName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void createTest() throws Exception {
    this.productRepositoryBean.create(new ProductRequest());
    Mockito.verify(this.pcbFeign)
        .createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createWithErrorTest() throws Exception {
    GdnBaseRestResponse baseResponseError =
        new GdnBaseRestResponse("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    Mockito.when(
            this.pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseResponseError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.create(new ProductRequest());
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .createNewProductWithSpecificationDetailGeneratedBySystem(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void createProductTest() throws Exception {
    GdnRestSingleResponse<MapResponse<String, String>> gdnRestSingleResponse = new GdnRestSingleResponse();
    gdnRestSingleResponse.setValue(new MapResponse<>(new HashMap<String, String>()));
    gdnRestSingleResponse.setSuccess(true);
    ProductRequest productRequest = new ProductRequest();
    Mockito.when(
            pcbFeign.createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID, GdnBaseLookup.DEFAULT_USERNAME, true, productRequest))
        .thenReturn(gdnRestSingleResponse);
    Map<String, String> result = this.productRepositoryBean.createProduct(productRequest, true);
    Mockito.verify(pcbFeign)
        .createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID, GdnBaseLookup.DEFAULT_USERNAME, true, productRequest);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(gdnRestSingleResponse.getValue().getMap(), result);
  }


  @Test
  public void createProductWithErrorTest() throws Exception {
    GdnRestSingleResponse<MapResponse<String, String>> baseResponseError = new GdnRestSingleResponse<>();
    baseResponseError.setSuccess(false);
    ProductRequest productRequest = new ProductRequest();
    Mockito.when(
            pcbFeign.createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID, GdnBaseLookup.DEFAULT_USERNAME, true, productRequest))
        .thenReturn(baseResponseError);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.createProduct(productRequest, true);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID, GdnBaseLookup.DEFAULT_USERNAME, true, productRequest);
    }
  }

  @Test
  public void createProductWithValidationErrorTest() throws Exception {
    GdnRestSingleResponse<MapResponse<String, String>> baseResponseError = new GdnRestSingleResponse<>();
    baseResponseError.setSuccess(false);
    baseResponseError.setErrorCode(ErrorCategory.VALIDATION.getCode());
    ProductRequest productRequest = new ProductRequest();
    Mockito.when(pcbFeign.createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID,
        GdnBaseLookup.DEFAULT_USERNAME, true, productRequest)).thenReturn(baseResponseError);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productRepositoryBean.createProduct(productRequest, true);
      });
    } finally {
      Mockito.verify(pcbFeign)
          .createProduct(DEFAULT_STORE_ID, CHANNEL_ID, CLIENT_ID, DEFAULT_REQUEST_ID, GdnBaseLookup.DEFAULT_USERNAME,
              true, productRequest);
    }
  }

  @Test
  public void updateActivatedTest() throws Exception {
    this.productRepositoryBean
        .updateActivated(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE, true);
    Mockito.verify(this.pcbFeign)
        .updateProductActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE), Mockito.anyBoolean());
  }

  @Test
  public void updateActivatedWithErrorTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.updateActivated(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2, true);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .updateProductActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_CODE_2), Mockito.anyBoolean());
  }

  @Test
  public void findDetailByIdTest() throws Exception {
    this.productRepositoryBean.findDetailById(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID);
    Mockito.verify(this.pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID), eq(true));
  }

  @Test
  public void findDetailByIdWithErrorTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.findDetailById(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(ProductRepositoryBeanTest.DEFAULT_PRODUCT_ID_2), eq(true));
  }

  @Test
  public void updateProductContentTest() throws Exception {
    ProductRequest request = new ProductRequest();
    this.productRepositoryBean.updateProductContent(request);
    Mockito.verify(this.pcbFeign)
        .updateProductContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void updateProductContentWithErrorTest() throws Exception {
    GdnBaseRestResponse baseResponseError =
        new GdnBaseRestResponse("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    Mockito.when(this.pcbFeign.updateProductContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.any()))
        .thenReturn(baseResponseError);
    try {
      ProductRequest request = new ProductRequest();
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.updateProductContent(request);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .updateProductContent(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void updateProductImageTest() throws Exception {
    ProductRequest request = new ProductRequest();
    this.productRepositoryBean.updateProductImage(request);
    Mockito.verify(this.pcbFeign)
        .updateProductImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateProductImageWithErrorTest() throws Exception {
    GdnBaseRestResponse baseResponseError =
        new GdnBaseRestResponse("Read Timeout", ErrorCategory.UNSPECIFIED.getCode(), false,
            ProductRepositoryBeanTest.DEFAULT_REQUEST_ID);
    Mockito.when(this.pcbFeign.updateProductImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(baseResponseError);
    try {
      ProductRequest request = new ProductRequest();
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productRepositoryBean.updateProductImage(request);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign)
        .updateProductImage(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void getProductDetailsByProductCodesTest() throws Exception {
    GdnRestListResponse<ProductDetailResponse> gdnRestListResponse =
        new GdnRestListResponse<>(null, null, true, REQUEST_ID);
    Mockito.when(
        pcbFeign.getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(StringUtils.EMPTY), Mockito.anyList(),
            eq(false), eq(true))).thenReturn(gdnRestListResponse);
    GdnRestListResponse<ProductDetailResponse> response = productRepositoryBean
        .getProductDetailsByProductCodes(DEFAULT_REQUEST_ID, StringUtils.EMPTY,
            new ArrayList<String>());
    Mockito.verify(pcbFeign)
        .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(StringUtils.EMPTY), Mockito.anyList(),
            eq(false), eq(true));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void getProductDetailsByProductCodesTest_FailedResponse() throws Exception {
    GdnRestListResponse<ProductDetailResponse> gdnRestListResponse =
        new GdnRestListResponse<>(null, null, false, null);
    Mockito.when(
        pcbFeign.getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(StringUtils.EMPTY), Mockito.anyList(),
            eq(false), eq(true))).thenReturn(gdnRestListResponse);
    try {
      GdnRestListResponse<ProductDetailResponse> response = productRepositoryBean
          .getProductDetailsByProductCodes(DEFAULT_REQUEST_ID, StringUtils.EMPTY,
              new ArrayList<String>());
    } catch (ApplicationException e) {
      Mockito.verify(pcbFeign)
          .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              eq(GdnMandatoryRequestParameterUtil.getRequestId()), eq(StringUtils.EMPTY),
              Mockito.anyList(), eq(false), eq(true));
    }
  }

  @Test
  public void isProductImagesActivatedTest() throws Exception {
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    productRepositoryBean.isProductImagesActivated(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void isProductImagesActivatedRequestIdUsernameEmptyTest() throws Exception {
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    productRepositoryBean.isProductImagesActivated(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void isProductImagesActivatedExceptionTest() throws Exception {
    ActivateImageResponse value = new ActivateImageResponse();
    GdnRestSingleResponse<ActivateImageResponse> response =
        new GdnRestSingleResponse<ActivateImageResponse>(null, null, false, value, getRequestId());
    Mockito.when(pcbFeign.isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productRepositoryBean.isProductImagesActivated(DEFAULT_PRODUCT_CODE);
      });
    } catch (Exception e) {
      Assertions.assertFalse(response.isSuccess());
      throw e;
    }
    Mockito.verify(pcbFeign)
        .isProductImagesActivated(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void activateAndUpdateImageNameTest() throws Exception {
    ActivateImageRequest request = new ActivateImageRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    Mockito.when(pcbFeign.activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    productRepositoryBean.activateAndUpdateImageName(request);
    Mockito.verify(pcbFeign)
        .activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void activateAndUpdateImageNameEmptyRequestIdUsernameEmptyTest() throws Exception {
    ActivateImageRequest request = new ActivateImageRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    MDC.remove(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    MDC.remove(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    Mockito.when(pcbFeign.activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    productRepositoryBean.activateAndUpdateImageName(request);
    Mockito.verify(pcbFeign)
        .activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void activateAndUpdateImageNameExceptionTest() throws Exception {
    ActivateImageRequest request = new ActivateImageRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    MDC.remove(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    MDC.remove(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    Mockito.when(pcbFeign.activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productRepositoryBean.activateAndUpdateImageName(request);
      });
    } catch (Exception e) {
      Assertions.assertFalse(response.isSuccess());
      throw e;
    }
    Mockito.verify(pcbFeign).activateAndUpdateImageName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void getProductItemByListOfProductCodeTest() throws Exception {
    List<ProductItemDetailResponse> productItemResponses = new ArrayList<>();
    ProductCodesRequest productCodesRequest = new ProductCodesRequest();
    productCodesRequest.setProductCodes(Collections.singletonList(PRODUCT_CODE));
    Mockito.when(productOutbound.getProductItemByListOfProductCode(productCodesRequestArgumentCaptor.capture(), eq(false), eq(true), eq(true)))
        .thenReturn(productItemResponses);
    productRepositoryBean.getProductItemByListOfProductCode(Collections.singletonList(PRODUCT_CODE), true, true);
    Mockito.verify(productOutbound).getProductItemByListOfProductCode(productCodesRequestArgumentCaptor.capture(), eq(false), eq(true), eq(true));
  }

  @Test
  public void testFindProductDetailByProductCode_withProductStateParam() throws Exception {
    ProductDetailResponse value = new ProductDetailResponse();
    value.setProductCode(DEFAULT_PRODUCT_CODE);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(Boolean.TRUE), eq(DEFAULT_PRODUCT_CODE), eq(true)))
        .thenReturn(response);
    ProductDetailResponse productDetailResponse =
        productRepositoryBean.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE, Boolean.TRUE);
    Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq(Boolean.TRUE), eq(DEFAULT_PRODUCT_CODE), eq(true));
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productDetailResponse.getProductCode());
  }

  @Test
  public void testFindProductBasicDetailByProductCode() throws Exception {
    ProductResponse value = new ProductResponse();
    value.setProductCode(DEFAULT_PRODUCT_CODE);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE), Mockito.anyBoolean()))
        .thenReturn(response);
    ProductResponse productResponse =
        productRepositoryBean.findProductBasicDetailByProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE),
            Mockito.anyBoolean());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productResponse.getProductCode());
  }

  @Test
  public void testFindProductDetailByProductCode() throws Exception {
    ProductDetailResponse value = new ProductDetailResponse();
    value.setProductCode(DEFAULT_PRODUCT_CODE);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<>(null, null, true, value, getRequestId());
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE), eq(true)))
        .thenReturn(response);
    ProductDetailResponse productDetailResponse =
        productRepositoryBean.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE), eq(true));
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, productDetailResponse.getProductCode());
  }

  @Test
  public void findProductDetailByProductCode_FailureTest() throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, null, getRequestId());
    Mockito.when(pcbFeign.getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE), eq(true)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productRepositoryBean.findProductDetailByProductCode(DEFAULT_PRODUCT_CODE);
      });
    }finally {
      Mockito.verify(pcbFeign)
          .getProductDetailByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(), eq(DEFAULT_PRODUCT_CODE), eq(true));
    }
  }

  @Test
  public void testUpdateRejectedProduct_success() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.TRUE);
    Mockito.when(pcbFeign.updateRejectedProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(GdnBaseLookup.DEFAULT_USERNAME), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    productRepositoryBean.updateRejectedProduct(new ProductRequest());
    Mockito.verify(pcbFeign)
        .updateRejectedProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(GdnBaseLookup.DEFAULT_USERNAME), Mockito.any(ProductRequest.class));
  }

  @Test
  public void testUpdateRejectedProduct_fail() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(Boolean.FALSE);
    Mockito.when(pcbFeign.updateRejectedProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(GdnBaseLookup.DEFAULT_USERNAME), Mockito.any(ProductRequest.class)))
        .thenReturn(response);
    try {
      productRepositoryBean.updateRejectedProduct(new ProductRequest());
    } catch (ApplicationException e) {
      Mockito.verify(pcbFeign)
          .updateRejectedProduct(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              eq(GdnBaseLookup.DEFAULT_USERNAME), Mockito.any(ProductRequest.class));
    }
  }

  @Test
  public void clearMasterProductCacheTest() throws Exception {
    Mockito.when(this.pcbFeign.clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnBaseRestResponse(true));
    productRepositoryBean.clearMasterProductCache(DEFAULT_PRODUCT_CODE, DEFAULT_PRODUCT_ID);
    Mockito.verify(this.pcbFeign)
        .clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void clearMasterProductCacheSyncTest() throws Exception {
    Mockito.when(this.pcbFeign.clearProductCacheSyncByProductIdAndProductCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(DEFAULT_REQUEST_ID), eq(GdnBaseLookup.DEFAULT_USERNAME), eq(PRODUCT_ID),
        eq(DEFAULT_PRODUCT_CODE))).thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    productRepositoryBean.clearMasterProductCacheSync(PRODUCT_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.pcbFeign)
        .clearProductCacheSyncByProductIdAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            eq(DEFAULT_REQUEST_ID), eq(GdnBaseLookup.DEFAULT_USERNAME), eq(PRODUCT_ID), eq(DEFAULT_PRODUCT_CODE));
  }

  @Test
  public void clearMasterProductCacheSyncTestException() throws Exception {
    Mockito.when(this.pcbFeign.clearProductCacheSyncByProductIdAndProductCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), eq(DEFAULT_REQUEST_ID), eq(GdnBaseLookup.DEFAULT_USERNAME), eq(PRODUCT_ID),
        eq(DEFAULT_PRODUCT_CODE))).thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try {
      productRepositoryBean.clearMasterProductCacheSync(PRODUCT_ID, DEFAULT_PRODUCT_CODE);
    } catch (ApplicationRuntimeException ex) {
      Mockito.verify(this.pcbFeign)
          .clearProductCacheSyncByProductIdAndProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              eq(DEFAULT_REQUEST_ID), eq(GdnBaseLookup.DEFAULT_USERNAME), eq(PRODUCT_ID), eq(DEFAULT_PRODUCT_CODE));
    }
  }

  @Test
  public void clearMasterProductCacheWithMDCTest() throws Exception {
    Mockito.when(this.pcbFeign.clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnBaseRestResponse(true));
    productRepositoryBean.clearMasterProductCache(DEFAULT_PRODUCT_CODE, DEFAULT_PRODUCT_ID);
    Mockito.verify(this.pcbFeign)
        .clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void clearMasterProductCacheExceptionTest() throws Exception {
    Mockito.when(this.pcbFeign.clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productRepositoryBean.clearMasterProductCache(DEFAULT_PRODUCT_CODE, DEFAULT_PRODUCT_ID);
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(this.pcbFeign)
        .clearProductCache(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void replaceProductImages() throws Exception {
    Mockito.when(pcbFeign.replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestSingleResponse<GeneratedProductImagesPathResponse>(null, DEFAULT_REQUEST_ID));

    productRepositoryBean.replaceProductImages(DEFAULT_REQUEST_ID, DEFAULT_USER_NAME,
        new ReplaceProductImagesRequest());

    Mockito.verify(pcbFeign)
        .replaceProductImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.any());
  }

  @Test
  public void movePrdCategoryByPrdCode_Valid_Success() throws Exception {
    Mockito.when(pcbFeign.movePrdCategoryByPrdCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<CategorySummaryResponse>(null, DEFAULT_REQUEST_ID));

    productRepositoryBean
        .movePrdCategoryByPrdCode(DEFAULT_REQUEST_ID, DEFAULT_USER_NAME, DEFAULT_PRODUCT_CODE,
            DEFAULT_CATEGORY_CODE);

    Mockito.verify(pcbFeign)
        .movePrdCategoryByPrdCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void addProductAttributesByProductCode_Valid_Success() throws Exception {
    Mockito.when(
            pcbFeign.addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<>(null, null, requestId));

    productRepositoryBean.addProductAttributesByProductCode(DEFAULT_REQUEST_ID, DEFAULT_USER_NAME,
        new AddProductAttributesRequest());

    Mockito.verify(pcbFeign)
        .addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void addProductAttributesByProductCode_ResponseFalse_ThrowException() throws Exception {
    Mockito.when(
            pcbFeign.addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, requestId));
    try {
      Assertions.assertThrows(Exception.class, () -> {
        productRepositoryBean.addProductAttributesByProductCode(DEFAULT_REQUEST_ID, DEFAULT_USER_NAME,
            new AddProductAttributesRequest());
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(pcbFeign)
        .addProductAttributesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void findProductResponseTest() throws Exception {
    ProductDetailResponse response = new ProductDetailResponse();
    response.setBrand("brand");
    GdnRestSingleResponse<ProductDetailResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>(response, StringUtils.EMPTY);
    Mockito.when(
        pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq("id"), eq(true))).thenReturn(gdnRestSingleResponse);
    ProductDetailResponse result = this.productRepositoryBean.findProductResponse("id");
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), eq("id"), eq(true));
    Assertions.assertEquals(response, result);
  }

  @Test
  public void updateSimpleMasterProduct() throws Exception {
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setUpdateSuccess(Boolean.TRUE);
    simpleMasterProductUpdateResponse.setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<SimpleMasterProductUpdateResponse> responseGdnRestSingleResponse=
    new GdnRestSingleResponse<>(simpleMasterProductUpdateResponse, DEFAULT_REQUEST_ID);
    Mockito.when(pcbFeign.updateSimpleMasterData(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleMasterProductUpdateRequest.class)))
        .thenReturn(responseGdnRestSingleResponse);
    SimpleMasterProductUpdateResponse response =
        productRepositoryBean.updateSimpleMasterProduct(simpleMasterProductUpdateRequestDTO);
    Mockito.verify(pcbFeign)
        .updateSimpleMasterData(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), simpleMasterProductUpdateRequestArgumentCaptor.capture());
    Assertions.assertNotNull(response);
    Assertions.assertEquals(PRODUCT_CODE, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getName());
    Assertions.assertEquals(BRAND, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(LENGTH, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getLength());
    Assertions.assertEquals(WEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getWeight());
    Assertions.assertEquals(WIDTH, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getWidth());
    Assertions.assertEquals(HEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getHeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getShippingWeight());
    Assertions.assertEquals(DANGEROUS_GOODS_LEVEL, simpleMasterProductUpdateRequestArgumentCaptor
        .getValue().getDangerousGoodsLevel());
  }

  @Test
  public void updateSimpleMasterProduct_Exception() throws Exception {
    GdnRestSingleResponse<SimpleMasterProductUpdateResponse> responseGdnRestSingleResponse=
        new GdnRestSingleResponse<>(ERROR_MESSAGE, ERROR_CODE, false, null, DEFAULT_REQUEST_ID);
    Mockito.when(pcbFeign.updateSimpleMasterData(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(SimpleMasterProductUpdateRequest.class)))
        .thenReturn(responseGdnRestSingleResponse);
    try {
      productRepositoryBean.updateSimpleMasterProduct(simpleMasterProductUpdateRequestDTO);
    }catch (Exception e){
      Assertions.assertEquals(ApplicationException.class, e.getClass());
    }
    Mockito.verify(pcbFeign)
        .updateSimpleMasterData(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), simpleMasterProductUpdateRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getName());
    Assertions.assertEquals(BRAND, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getBrand());
    Assertions.assertEquals(LENGTH, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getLength());
    Assertions.assertEquals(WEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getWeight());
    Assertions.assertEquals(WIDTH, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getWidth());
    Assertions.assertEquals(HEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getHeight());
    Assertions.assertEquals(SHIPPING_WEIGHT, simpleMasterProductUpdateRequestArgumentCaptor.getValue().getShippingWeight());
    Assertions.assertEquals(DANGEROUS_GOODS_LEVEL, simpleMasterProductUpdateRequestArgumentCaptor
        .getValue().getDangerousGoodsLevel());
  }

  @Test
  public void getCatalogByTypeTest() throws Exception {
    CatalogType catalogType = CatalogType.MASTER_CATALOG;
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogCode(CATALOG_CODE);
    GdnRestListResponse<CatalogResponse> response = new GdnRestListResponse<>();
    response.setContent(Arrays.asList(catalogResponse));
    response.setSuccess(true);
    Mockito.when(pcbFeign.getCatalogByType(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), 0, 10, catalogType)).thenReturn(response);
    productRepositoryBean.getCatalogByType(CatalogType.MASTER_CATALOG, DEFAULT_PAGEABLE);
    Mockito.verify(pcbFeign).getCatalogByType(GdnMandatoryRequestParameterUtil.getStoreId(),
      GdnMandatoryRequestParameterUtil.getChannelId(),
      GdnMandatoryRequestParameterUtil.getClientId(),
      GdnMandatoryRequestParameterUtil.getRequestId(),
      GdnMandatoryRequestParameterUtil.getUsername(), 0, 10, catalogType);
    Assertions.assertEquals(CATALOG_CODE, response.getContent().get(0).getCatalogCode());
  }

  @Test
  public void getPredictionListByCategoryCodeTest() throws Exception {
    GdnRestListResponse<ProductPredictionCategoryMappingResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.singletonList(new ProductPredictionCategoryMappingResponse()));
    response.setSuccess(true);
    Mockito.when(pcbFeign.getPredictionListByCategoryCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    productRepositoryBean.getPredictionListByCategoryCode(CATALOG_CODE);
    Mockito.verify(pcbFeign)
        .getPredictionListByCategoryCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getCatalogByTypeExceptionTest() throws Exception {
    CatalogType catalogType = CatalogType.MASTER_CATALOG;
    Mockito.when(
        pcbFeign.getCatalogByType(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(catalogType))).thenReturn(null);
    try {
      productRepositoryBean.getCatalogByType(CatalogType.MASTER_CATALOG, DEFAULT_PAGEABLE);
    } catch (Exception e) {
      Mockito.verify(pcbFeign)
          .getCatalogByType(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), eq(catalogType));
    }
  }

  @Test
  public void updateProductAndItemImages() throws Exception {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, getProductDetailResponse(),
            StringUtils.EMPTY);
    MDC.put(USER_NAME, DEFAULT_USER_NAME);
    Mockito.when(
        pcbFeign.updateProductAndItemImagesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.any(ProductAndItemImageRequest.class))).thenReturn(response);
    productRepositoryBean.updateProductAndItemImages(productAndItemImageRequest);
    Mockito.verify(pcbFeign)
        .updateProductAndItemImagesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            productAndItemImageRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productAndItemImageRequestArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void getConfigurationStatus() throws Exception {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, configurationStatusResponses,
            new PageMetaData(), StringUtils.EMPTY);
    MDC.put(USER_NAME, DEFAULT_USER_NAME);
    MDC.put(REQUEST_ID, REQUEST_ID);
    Mockito.when(pcbFeign.getReviewConfiguration(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(configurationStatusRequestList))).thenReturn(response);
    productRepositoryBean.getConfigurationStatus(configurationStatusRequestList);
    Mockito.verify(pcbFeign)
        .getReviewConfiguration(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), eq(configurationStatusRequestList));
  }

  @Test
  public void getConfigurationStatusExceptionTest() throws Exception {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, configurationStatusResponses,
            new PageMetaData(), StringUtils.EMPTY);
    MDC.put(USER_NAME, DEFAULT_USER_NAME);
    MDC.put(REQUEST_ID, REQUEST_ID);
    Mockito.when(pcbFeign.getReviewConfiguration(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(configurationStatusRequestList))).thenReturn(response);
    try {
      productRepositoryBean.getConfigurationStatus(configurationStatusRequestList);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pcbFeign)
          .getReviewConfiguration(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), eq(configurationStatusRequestList));
    }
  }

  @Test
  public void updateProductAndItemImagesExceptionTest() throws Exception {
    ProductAndItemImageRequest productAndItemImageRequest = new ProductAndItemImageRequest();
    productAndItemImageRequest.setProductCode(PRODUCT_CODE);
    MDC.put(USER_NAME, DEFAULT_USER_NAME);
    GdnRestSingleResponse<ProductDetailResponse> response =
        new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, getProductDetailResponse(),
            StringUtils.EMPTY);
    Mockito.when(
        pcbFeign.updateProductAndItemImagesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.any(ProductAndItemImageRequest.class))).thenReturn(response);
    try {
      productRepositoryBean.updateProductAndItemImages(productAndItemImageRequest);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pcbFeign)
          .updateProductAndItemImagesByProductCode(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
              productAndItemImageRequestArgumentCaptor.capture());
      Assertions.assertEquals(PRODUCT_CODE, productAndItemImageRequestArgumentCaptor.getValue().getProductCode());
    }
  }

  @Test
  public void getAllProductDetailsByProductCodesTest() throws Exception {
    GdnRestListResponse<ProductDetailResponse> gdnRestListResponse = new GdnRestListResponse<>(null, null, true, null);
    Mockito.when(pcbFeign
        .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), eq(true), eq(true)))
        .thenReturn(gdnRestListResponse);
    GdnRestListResponse<ProductDetailResponse> response = productRepositoryBean
        .getAllProductDetailsByProductCodes(DEFAULT_REQUEST_ID, StringUtils.EMPTY, new ArrayList<String>());
    Mockito.verify(pcbFeign)
        .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), eq(true), eq(true));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void getAllProductDetailsByProductCodesTest_FailedResponse() throws Exception {
    GdnRestListResponse<ProductDetailResponse> gdnRestListResponse = new GdnRestListResponse<>(null, null, false, null);
    Mockito.when(pcbFeign
        .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), eq(true), eq(true)))
        .thenReturn(gdnRestListResponse);
    try {
      GdnRestListResponse<ProductDetailResponse> response = productRepositoryBean
          .getAllProductDetailsByProductCodes(DEFAULT_REQUEST_ID, StringUtils.EMPTY, new ArrayList<String>());
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pcbFeign)
          .getAllProductDetailListByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), eq(true), eq(true));
    }
  }

  @Test
  public void takeDownProductBasedOnBrandTest() throws Exception {
    SimpleBooleanResponse simpleBooleanResponse = new SimpleBooleanResponse();
    simpleBooleanResponse.setResult(false);
    Mockito.when(pcbFeign.takeDownProductBasedOnBrand(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new ProductBrandValidationRequest())).thenReturn(new GdnRestSingleResponse(null, null, true, simpleBooleanResponse, null));
    productRepositoryBean.takeDownProductBasedOnBrand(new ProductBrandValidationRequest());
    Mockito.verify(pcbFeign).takeDownProductBasedOnBrand(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        new ProductBrandValidationRequest());
  }

  @Test
  public void discardProductTest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    Mockito.when(pcbFeign.discardProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, GdnMandatoryRequestParameterUtil.getRequestId()));
    productRepositoryBean.discardProduct(productRequest);
    Mockito.verify(pcbFeign)
        .discardProduct(GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productRequest);
  }

  @Test
  public void getCategoryRestrictedKeywordDetailTest() throws Exception {
    Mockito.when(pcbFeign
        .getCategoryRestrictedKeywordDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_PRODUCT_ID))
        .thenReturn(new GdnRestSingleResponse(null, null, true, null, null));
    productRepositoryBean.getCategoryRestrictedKeywordDetail(DEFAULT_PRODUCT_ID);
    Mockito.verify(pcbFeign)
        .getCategoryRestrictedKeywordDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_PRODUCT_ID);
  }

  @Test
  public void autoFillProductAttributeTest() throws Exception {
    Mockito.when(
        pcbFeign.autoFillProductAttribute(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            PRODUCT_CODE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));
    productRepositoryBean.autoFillProductAttribute(DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(pcbFeign)
        .autoFillProductAttribute(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            PRODUCT_CODE);
  }

  @Test
  public void getCategoryRestrictedKeywordDetailListTest() throws Exception {
    Mockito.when(
        pcbFeign.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            new CategoryCodeAndKeywordListRequest(CATALOG_CODE, Collections.singletonList(CHANNEL_ID)))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));
    productRepositoryBean.getCategoryRestrictedKeywordDetailList(CATALOG_CODE, Collections.singletonList(CHANNEL_ID));
    Mockito.verify(pcbFeign)
        .getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            new CategoryCodeAndKeywordListRequest(CATALOG_CODE, Collections.singletonList(CHANNEL_ID)));
  }

  @Test
  public void updateProductMasterDataAndImagesAndUpcCodeTest() throws Exception {
    GdnRestSingleResponse<EditProductItemAndImageResponse> response =
      new GdnRestSingleResponse(null, null, true, null, null);
    EditProductDetailRequest request = EditProductDetailRequest.builder().build();
    Mockito.when(pcbFeign.updateProductMasterDataAndImagesAndUpcCode(DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE, true,request))
      .thenReturn(response);
    productRepositoryBean.updateProductMasterDataAndImagesAndUpcCode(PRODUCT_CODE,
      true, request);
    Mockito.verify(pcbFeign)
      .updateProductMasterDataAndImagesAndUpcCode(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE, true,
        request);
  }

  @Test
  public void updateProductMasterDataAndImagesAndUpcCodeExceptionTest() throws Exception {
    GdnRestSingleResponse<EditProductItemAndImageResponse> response =
      new GdnRestSingleResponse(null, null, false, null, null);
    EditProductDetailRequest request = EditProductDetailRequest.builder().build();
    Mockito.when(pcbFeign.updateProductMasterDataAndImagesAndUpcCode(DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE, true, request))
      .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        productRepositoryBean.updateProductMasterDataAndImagesAndUpcCode(
            PRODUCT_CODE, true, request);
      });
    } finally {
      Mockito.verify(pcbFeign)
        .updateProductMasterDataAndImagesAndUpcCode(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), PRODUCT_CODE, true, request);
    }
  }

  @Test
  public void updateProductBrandDataTest() throws Exception {
    GdnRestSingleResponse<ProductBrandUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new ProductBrandUpdateResponse(), null);
    ProductBrandUpdateRequest request = new ProductBrandUpdateRequest();
    Mockito.when(
            pcbFeign.updateProductBrandData(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request))
        .thenReturn(response);
    productRepositoryBean.updateProductBrandData(request);
    Mockito.verify(pcbFeign)
        .updateProductBrandData(DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
  }

  @Test
  public void testPrepareProductRequestForPCBUpdate() throws Exception {
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() - 1);    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
        this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testPrepareProductRequestForPCBUpdateWithNoDTO() throws Exception {
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() - 1);    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
        this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        null, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testPrepareProductRequestForPCBUpdate_WithDTO() throws Exception {
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() - 1);
    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
        this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testPrepareProductRequestForPCBUpdate_WithDTO2() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "handleAddDeleteVariantMismatch",true);
    Product product = getProduct();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() + 1);
    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
        this.pcbFeign.getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, new ArrayList<>(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
        .getProductDetailWithOriginalImages(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testPrepareProductRequestForPCBUpdateWithHandleAddDeleteVariantMismatch() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "handleAddDeleteVariantMismatch",true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE_1);
    attributeMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_2);
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() + 1);
    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    ProductItemAttributeValue productItemAttributeValueRequest1 =
      new ProductItemAttributeValue();
    Attribute attributeRequest1 = new Attribute();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeRequest1.setVariantCreation(true);
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue(ATTRIBUTE_VALUE_1);
    ProductItemAttributeValue productItemAttributeValueRequest2 =
      new ProductItemAttributeValue();
    Attribute attributeRequest2 = new Attribute();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setVariantCreation(true);
    productItemAttributeValueRequest2.setAttribute(attributeRequest2);
    productItemAttributeValueRequest2.setValue(ATTRIBUTE_VALUE_2);
    List<ProductItemAttributeValue> productItemAttributeValueRequests = new ArrayList<>();
    productItemAttributeValueRequests.add(productItemAttributeValueRequest1);
    productItemAttributeValueRequests.add(productItemAttributeValueRequest2);
    productItemLevel3.setItemName(GENERATED_ITEM_NAME_1);
    product.getProductItems().get(0).setProductItemAttributeValues(productItemAttributeValueRequests);
    newlyAddedItems.add(productItemLevel3);
    productItemLevel3.setItemAttributesMap(attributeMap);
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
      this.pcbFeign.getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
       Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true)).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, Collections.emptyList(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true);
  }

  @Test
  public void testPrepareProductRequestForPCBUpdateWithHandleAddDeleteVariantMismatchDifferntAttributes() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "handleAddDeleteVariantMismatch",true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_3);
    attributeMap.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE_4);
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() + 1);
    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    ProductItemAttributeValue productItemAttributeValueRequest1 =
      new ProductItemAttributeValue();
    Attribute attributeRequest1 = new Attribute();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeRequest1.setVariantCreation(false);
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue(ATTRIBUTE_VALUE_1);
    ProductItemAttributeValue productItemAttributeValueRequest2 =
      new ProductItemAttributeValue();
    Attribute attributeRequest2 = new Attribute();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setVariantCreation(false);
    productItemAttributeValueRequest2.setAttribute(attributeRequest2);
    productItemAttributeValueRequest2.setValue(ATTRIBUTE_VALUE_2);
    List<ProductItemAttributeValue> productItemAttributeValueRequests = new ArrayList<>();
    productItemAttributeValueRequests.add(productItemAttributeValueRequest1);
    productItemAttributeValueRequests.add(productItemAttributeValueRequest2);
    productItemLevel3.setItemName(GENERATED_ITEM_NAME_1);
    product.getProductItems().get(0).setProductItemAttributeValues(productItemAttributeValueRequests);
    newlyAddedItems.add(productItemLevel3);
    productItemLevel3.setItemAttributesMap(attributeMap);
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
      this.pcbFeign.getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true)).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, Collections.emptyList(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true);
  }

  @Test
  public void testPrepareProductRequestForPCBUpdateWithHandleAddDeleteVariantMismatchDifferntNonAttributes() throws Exception {
    ReflectionTestUtils.setField(productRepositoryBean, "handleAddDeleteVariantMismatch",true);
    Product product = getProduct();
    product.setId(PRODUCT_ID);
    TreeMap<String, String> attributeMap = new TreeMap<>();
    attributeMap.put(ATTRIBUTE_CODE_2, ATTRIBUTE_VALUE_3);
    attributeMap.put(ATTRIBUTE_CODE_1, ATTRIBUTE_VALUE_4);
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    BeanUtils.copyProperties(product, productDetailResponse);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
      Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    productDetailResponse.setProductItemResponses(new HashSet<ProductItemResponse>());
    productDetailResponse.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productDetailResponse.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productDetailResponse.setVersion(3L);
    product.setVersion(productDetailResponse.getVersion() + 1);
    List<String> deletedItems = new ArrayList<>();
    GdnRestSingleResponse<ProductDetailResponse> response =
      new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        getProductDetailResponse(), StringUtils.EMPTY);
    response.setValue(productDetailResponse);
    Boolean isPristineCategory = true;
    boolean scoreUpdated = true;
    List<ProductItemLevel3> newlyAddedItems =
      new ArrayList<>();
    ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
    ProductItemAttributeValue productItemAttributeValueRequest1 =
      new ProductItemAttributeValue();
    Attribute attributeRequest1 = new Attribute();
    attributeRequest1.setAttributeCode(ATTRIBUTE_CODE_1);
    attributeRequest1.setVariantCreation(false);
    productItemAttributeValueRequest1.setAttribute(attributeRequest1);
    productItemAttributeValueRequest1.setValue(ATTRIBUTE_VALUE_1);
    ProductItemAttributeValue productItemAttributeValueRequest2 =
      new ProductItemAttributeValue();
    Attribute attributeRequest2 = new Attribute();
    attributeRequest2.setAttributeCode(ATTRIBUTE_CODE_2);
    attributeRequest2.setVariantCreation(false);
    productItemAttributeValueRequest2.setAttribute(attributeRequest2);
    productItemAttributeValueRequest2.setValue(ATTRIBUTE_VALUE_2);
    List<ProductItemAttributeValue> productItemAttributeValueRequests = new ArrayList<>();
    productItemAttributeValueRequests.add(productItemAttributeValueRequest1);
    productItemAttributeValueRequests.add(productItemAttributeValueRequest2);
    productItemLevel3.setItemName(GENERATED_ITEM_NAME_1);
    product.getProductItems().get(0).setProductItemAttributeValues(productItemAttributeValueRequests);
    newlyAddedItems.add(productItemLevel3);
    productItemLevel3.setItemAttributesMap(attributeMap);
    Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap =
      new HashMap<>();
    com.gda.mta.product.dto.ProductDetailEditDTO productDetailEditDTO =
      new com.gda.mta.product.dto.ProductDetailEditDTO();
    Mockito.when(
      this.pcbFeign.getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true)).thenReturn(response);
    ProductRequest result =
      productRepositoryBean.prepareProductRequestForPCBUpdate(product, deletedItems,
        isPristineCategory, scoreUpdated, newlyAddedItems, productItemAttributeValueRequestMap,
        productDetailEditDTO, Collections.emptyList(), new ProductL3Response(), EditProductResponse.builder().build());
    Mockito.verify(pcbFeign)
      .getProductDetailWithOriginalImages(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, PRODUCT_ID, true);
  }

  // ===============================
  // Tests for getProductsByBrandName method
  // ===============================

  @Test
  public void getProductsByBrandName_Success_NonEmptyContent() throws Exception {
    // Given
    String brandName = "TestBrand";
    PageRequest pageable = PageRequest.of(0, 10);
    
    // Create ProductCodeResponse data
    ProductCodeResponse productCodeResponse1 = new ProductCodeResponse();
    productCodeResponse1.setId("PRODUCT_ID_1");
    productCodeResponse1.setProductCode("PRODUCT_CODE_1");
    
    ProductCodeResponse productCodeResponse2 = new ProductCodeResponse();
    productCodeResponse2.setId("PRODUCT_ID_2");
    productCodeResponse2.setProductCode("PRODUCT_CODE_2");
    
    List<ProductCodeResponse> productCodeResponseList = Arrays.asList(productCodeResponse1, productCodeResponse2);
    
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(2);
    pageMetaData.setPageSize(10);
    pageMetaData.setPageNumber(0);
    
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true); // Success is false - this is the only way method succeeds!
    response.setContent(productCodeResponseList); // Non-empty content
    response.setPageMetaData(pageMetaData);
    
    Mockito.when(pcbFeign.getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    )).thenReturn(response);
    
    // When
    Page<ProductCodeResponse> result = productRepositoryBean.getProductsByBrandName(brandName, pageable);
    
    // Then
    Assertions.assertNotNull(result);
    Assertions.assertEquals(2, result.getTotalElements());
    Assertions.assertEquals(2, result.getContent().size());
    Assertions.assertEquals("PRODUCT_ID_1", result.getContent().get(0).getId());
    Assertions.assertEquals("PRODUCT_CODE_1", result.getContent().get(0).getProductCode());
    Assertions.assertEquals("PRODUCT_ID_2", result.getContent().get(1).getId());
    Assertions.assertEquals("PRODUCT_CODE_2", result.getContent().get(1).getProductCode());
    
    Mockito.verify(pcbFeign).getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    );
  }

  @Test
  public void getProductsByBrandName_Failure_SuccessfulResponseWithContent() throws Exception {
    // Given - Testing the implementation bug where success=true triggers exception
    String brandName = "TestBrand";
    PageRequest pageable = PageRequest.of(0, 10);
    
    // Create ProductCodeResponse data
    ProductCodeResponse productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setId("PRODUCT_ID_1");
    productCodeResponse.setProductCode("PRODUCT_CODE_1");
    
    List<ProductCodeResponse> productCodeResponseList = Arrays.asList(productCodeResponse);
    
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false); // Success is true - will cause exception due to implementation bug
    response.setContent(productCodeResponseList);
    response.setErrorMessage("This should not happen");
    
    Mockito.when(pcbFeign.getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    )).thenReturn(response);
    
    // When & Then
    ApplicationException exception = Assertions.assertThrows(
        ApplicationException.class,
        () -> productRepositoryBean.getProductsByBrandName(brandName, pageable)
    );

    
    Mockito.verify(pcbFeign).getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    );
  }

  @Test
  public void getProductsByBrandName_Failure_EmptyContent() throws Exception {
    // Given
    String brandName = "NonExistentBrand";
    PageRequest pageable = PageRequest.of(0, 10);
    
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    response.setContent(Collections.emptyList()); // Empty content will cause exception
    response.setErrorMessage("No products found for brand");
    
    Mockito.when(pcbFeign.getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    )).thenReturn(response);
    
    // When & Then
    ApplicationException exception = Assertions.assertThrows(
        ApplicationException.class,
        () -> productRepositoryBean.getProductsByBrandName(brandName, pageable)
    );
    
    Mockito.verify(pcbFeign).getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    );
  }

  @Test
  public void getProductsByBrandName_Failure_ServiceException() throws Exception {
    // Given
    String brandName = "TestBrand";
    PageRequest pageable = PageRequest.of(0, 10);
    
    Mockito.when(pcbFeign.getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    )).thenThrow(new RuntimeException("PCB service timeout"));
    
    // When & Then
    RuntimeException exception = Assertions.assertThrows(
        RuntimeException.class,
        () -> productRepositoryBean.getProductsByBrandName(brandName, pageable)
    );
    
    Assertions.assertEquals("PCB service timeout", exception.getMessage());
    
    Mockito.verify(pcbFeign).getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    );
  }

  @Test
  public void getProductsByBrandName_Failure_SuccessfulResponseWithEmptyContent() throws Exception {
    // Given - Testing when success=true AND content is empty (both conditions true)
    String brandName = "TestBrand";
    PageRequest pageable = PageRequest.of(0, 10);
    
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true); // Success is true
    response.setContent(Collections.emptyList()); // Empty content - both conditions trigger exception
    response.setErrorMessage("Successful response but empty content");
    
    Mockito.when(pcbFeign.getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    )).thenReturn(response);

    productRepositoryBean.getProductsByBrandName(brandName, pageable);
    

    Mockito.verify(pcbFeign).getProductsByBrandName(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(Constants.DEFAULT_CHANNEL_ID),
        eq(Constants.DEFAULT_CLIENT_ID),
        Mockito.anyString(),
        Mockito.anyString(),
        eq(brandName),
        eq(0),
        eq(10)
    );
  }


}
