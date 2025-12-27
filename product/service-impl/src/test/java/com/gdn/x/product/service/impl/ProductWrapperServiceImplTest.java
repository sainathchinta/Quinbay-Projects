package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductWrapperServiceImplTest {

  private static final String REQUEST_ID = "request_id";
  private static final String PRODUCT_SKU = "product_sku";
  private static final String PRODUCT_NOT_FOUND = "Product not found";
  private static final String STORE_ID = "STORE_ID";
  private static final String TYPE = "ACTIVE";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";

  @InjectMocks
  private ProductWrapperServiceImpl productWrapperService;

  @Mock
  private ProductServiceImpl productService;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private ItemPickupPointWrapperServiceImpl itemPickupPointWrapperService;

  private ProductDetailPageEditRequest productDetailPageEditRequest;
  private ProductEditRequest productEditRequest;
  private ProductRequest productRequest;
  private ItemPickupPointUpdateRequest itemPickupPointUpdateRequest;
  private ItemViewConfigAndItemSkuRequest viewConfigAndItemSkuRequest;
  private List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequestList;
  private Map<String, ItemViewConfig> itemViewConfigMap;
  private Product product;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    product = new Product();
    productDetailPageEditRequest = new ProductDetailPageEditRequest();
    productRequest = new ProductRequest();
    productEditRequest = new ProductEditRequest();
    itemViewConfigMap = new HashMap<>();
    itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
    itemViewConfigAndItemSkuRequestList = new ArrayList<>();
    viewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequestList.add(viewConfigAndItemSkuRequest);
    productEditRequest.setProductRequest(productRequest);
    productEditRequest.setProductSku(PRODUCT_SKU);
    productEditRequest.setItemViewConfigAndItemSkuListRequest(itemViewConfigAndItemSkuRequestList);
    itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
    productDetailPageEditRequest.setProductEditRequest(productEditRequest);
    productDetailPageEditRequest.setItemPickupPointUpdateRequest(itemPickupPointUpdateRequest);
    productDetailPageEditRequest.setEditChangeType(EditChangeType.CONTENT_AND_ITEM_PICKUP_POINT);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.modelConverter);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointWrapperService);
    Mockito.verifyNoMoreInteractions(this.objectConverterService);
  }

  @Test
  public void updateEditedProductCombinedTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(new EditItemResponse());
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
            productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(null);
    CombinedEditItemResponse combinedEditItemResponse =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToProduct(productRequest);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedProductRequestNullTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = null;
    productDetailPageEditRequest.getProductEditRequest().setProductRequest(null);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true, null,
            productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    CombinedEditItemResponse response =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(new EditItemResponse());
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true, null,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedApiErrorCodeNullNullTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(null);
    productDetailPageEditRequest.getProductEditRequest().setProductRequest(null);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(productService.updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true,
        editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(editItemResponse);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    CombinedEditItemResponse response =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true, editProductDetailDTO,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedApiErrorCodeNullTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    productDetailPageEditRequest.getProductEditRequest().setProductRequest(null);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(productService.updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true,
        editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(editItemResponse);
    CombinedEditItemResponse response =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true, editProductDetailDTO,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedApiErrorCodeNonNullNullTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    productDetailPageEditRequest.getProductEditRequest().setProductRequest(null);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(productService.updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true,
        editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    CombinedEditItemResponse response =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(editItemResponse);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, null, itemViewConfigMap, true, true, editProductDetailDTO,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedEmptyViewConfigRequestTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    productDetailPageEditRequest.getProductEditRequest().setItemViewConfigAndItemSkuListRequest(new ArrayList<>());
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true,
            editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(new EditItemResponse());
    CombinedEditItemResponse response =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).convertToProduct(productRequest);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true, editProductDetailDTO,
            productEditRequest);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedL5ExceptionTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    productDetailPageEditRequest.getProductEditRequest().setItemViewConfigAndItemSkuListRequest(new ArrayList<>());
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true,
            editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, PRODUCT_NOT_FOUND));
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () ->
          this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
              PRODUCT_SKU, null));
    } finally {
      Mockito.verify(modelConverter).convertToProduct(productRequest);
      Mockito.verify(productService)
          .updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true,
              editProductDetailDTO, productEditRequest);
      Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
      Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
      Mockito.verify(productService)
          .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateEditedProductCombinedL5GenericExceptionTest() throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    productDetailPageEditRequest.getProductEditRequest().setItemViewConfigAndItemSkuListRequest(new ArrayList<>());
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true,
            editProductDetailDTO, productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenThrow(Exception.class);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    try {
      Assertions.assertThrows(Exception.class, () ->
          this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
              PRODUCT_SKU, null));
    } finally {
      Mockito.verify(modelConverter).convertToProduct(productRequest);
      Mockito.verify(productService)
          .updateEditedProduct(REQUEST_ID, product, new HashMap<>(), true, true,
              editProductDetailDTO, productEditRequest);
      Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
      Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
      Mockito.verify(productService)
          .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateEditedProductCombinedExceptionTest() throws Exception {
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(
            productService.updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true
                , null,
                productEditRequest))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, PRODUCT_NOT_FOUND));
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(null);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () ->
          this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
              PRODUCT_SKU, null));
    } finally {
      Mockito.verify(modelConverter).convertToProduct(productRequest);
      Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
      Mockito.verify(productService)
          .updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
              productEditRequest);
      Mockito.verify(productService)
          .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateEditedProductCombinedGenericExceptionTest() throws Exception {
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
            productEditRequest)).thenThrow(new Exception());
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(null);
    try {
      Assertions.assertThrows(Exception.class, () ->
          this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
              PRODUCT_SKU, null));
    } finally {
      Mockito.verify(modelConverter).convertToProduct(productRequest);
      Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
      Mockito.verify(productService)
          .updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
              productEditRequest);
      Mockito.verify(productService)
          .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    }
  }

  @Test
  public void updateEditedProductCombinedEditChangeTypeContentTest() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    productDetailPageEditRequest.setEditChangeType(EditChangeType.CONTENT);
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    Mockito.when(modelConverter.convertToProduct(productRequest)).thenReturn(product);
    Mockito.when(modelConverter.convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList))
        .thenReturn(itemViewConfigMap);
    Mockito.when(
        productService.updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
            productEditRequest)).thenReturn(editProductDetailDTO);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(null);
    Mockito.when(productService.updateProductScoreOnMasterDataChange(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any()))
        .thenReturn(new ArrayList<>(Collections.singletonList(productAndItemsVO)));
    CombinedEditItemResponse combinedEditItemResponse =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, STORE_ID, STORE_ID));
    Mockito.verify(modelConverter).convertToProduct(productRequest);
    Mockito.verify(modelConverter).convertToItemViewConfigMap(itemViewConfigAndItemSkuRequestList);
    Mockito.verify(productService)
        .updateEditedProduct(REQUEST_ID, product, itemViewConfigMap, true, true, null,
            productEditRequest);
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.verify(productService)
        .updateProductScoreOnMasterDataChange(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
            Mockito.anyString(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).processFinalSaveAndPublishEventForContentEditOnly(Mockito.any());
  }

  @Test
  public void updateEditedProductCombinedEditChangeTypeItemPickupPointTest() throws Exception {
    productDetailPageEditRequest.setEditChangeType(EditChangeType.ITEM_PICKUP_POINT);
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(null, null, editProductDetailDTO))
        .thenReturn(new EditItemResponse());
    Mockito.when(modelConverter.covertToItemPickupPointUpdateRequestVo(Mockito.any())).thenReturn(null);
    Mockito.when(
        productService.generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(),
            Mockito.any())).thenReturn(editProductDetailDTO);
    CombinedEditItemResponse combinedEditItemResponse =
        this.productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
            PRODUCT_SKU, null);
    Mockito.verify(modelConverter).covertToItemPickupPointUpdateRequestVo(Mockito.any());
    Mockito.verify(itemPickupPointWrapperService).updateItemPickupPoint(null, null, editProductDetailDTO);
    Mockito.verify(objectConverterService).toCombinedEditItemResponse(Mockito.any());
    Mockito.verify(productService)
        .generateEditProductDetailDTO(Mockito.anyBoolean(), Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }
  @Test
  public void getProductCountByTypeTest() {
    ReflectionTestUtils.setField(productWrapperService, "fetchProductCountFromCache", false);
    Mockito.when(productSearchService.getProductCountByType(TYPE, MERCHANT_CODE, true))
        .thenReturn(new ProductCountResponseVo(10L, 10L, 10L, 10L));
    ProductCountResponseVo productCountResponseVo =
        productWrapperService.getProductCountByType(TYPE, MERCHANT_CODE, true);
    Assertions.assertEquals(10L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getOutOfStock().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getSuspended().longValue());
    Mockito.verify(productSearchService).getProductCountByType(TYPE, MERCHANT_CODE, true);
  }

  @Test
  public void getProductCountByTypeCacheableTest() {
    ReflectionTestUtils.setField(productWrapperService, "fetchProductCountFromCache", true);
    Mockito.when(productSearchService.getProductCountByTypeCacheable(TYPE, MERCHANT_CODE, true))
        .thenReturn(new ProductCountResponseVo(10L, 10L, 10L, 10L));
    ProductCountResponseVo productCountResponseVo =
        productWrapperService.getProductCountByType(TYPE, MERCHANT_CODE, true);
    Assertions.assertEquals(10L, productCountResponseVo.getActive().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getOutOfStock().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getArchived().longValue());
    Assertions.assertEquals(10L, productCountResponseVo.getSuspended().longValue());
    Mockito.verify(productSearchService).getProductCountByTypeCacheable(TYPE, MERCHANT_CODE, true);
  }
}
