package com.gdn.x.product.service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductWrapperService;
import com.gdn.x.product.service.util.ModelConverter;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductWrapperServiceImpl implements ProductWrapperService {
  @Autowired
  private ProductService productService;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ProductSearchService productSearchService;

  @Value("${fetch.product.count.from.cache}")
  private boolean fetchProductCountFromCache;

  @Override
  public CombinedEditItemResponse updateEditedProductCombined(String requestId, boolean updateCategory,
      ProductDetailPageEditRequest productDetailPageEditRequest, String productSku,
      MandatoryRequestParam mandatoryRequestParam) throws Exception {
    validateProductDetailPageEditRequest(productDetailPageEditRequest);
    Long l3Version = null;
    EditProductDetailDTO editProductDetailDTO = null;
    EditItemResponse editItemResponse = null;
    try {
      switch (productDetailPageEditRequest.getEditChangeType()) {
        case CONTENT:
          l3Version =
              processContentUpdateForCombinedEdit(requestId, updateCategory, productDetailPageEditRequest, productSku,
                  mandatoryRequestParam);
          break;
        case ITEM_PICKUP_POINT:
          editItemResponse =
              processItemPickupPointUpdateForCombinedEdit(requestId, updateCategory, productDetailPageEditRequest,
                  productSku, mandatoryRequestParam);
          break;
        case CONTENT_AND_ITEM_PICKUP_POINT:
          editItemResponse = processContentAndItemPickupPointUpdateForCombinedEdit(requestId, updateCategory,
              productDetailPageEditRequest, productSku, mandatoryRequestParam);
          break;
        default:
          break;
      }
      CombinedEditItemResponse combinedEditItemResponse = new CombinedEditItemResponse();
      if (EditChangeType.CONTENT.equals(productDetailPageEditRequest.getEditChangeType())) {
        combinedEditItemResponse.setL3Version(l3Version);
        return combinedEditItemResponse;
      } else {
        return objectConverterService.toCombinedEditItemResponse(editItemResponse);
      }
    } catch (ApplicationRuntimeException e) {
      log.error("Error caught while updating product for productSku : {} ", productSku);
      throw new ApiIncorrectInputDataException(e.getErrorMessage(), e.getErrorCodes().getCode());
    } catch (Exception e) {
      log.error("Error occurred while performing combined update for edited product for productSku : {} , error is : ",
          productSku, e);
      throw e;
    }
  }

  @Override
  public ProductCountResponseVo getProductCountByType(String type, String merchantCode, boolean isActiveCounts) {
    if (fetchProductCountFromCache) {
      return productSearchService.getProductCountByTypeCacheable(type, merchantCode, isActiveCounts);
    } else {
      return productSearchService.getProductCountByType(type, merchantCode, isActiveCounts);
    }
  }

  private EditItemResponse processContentAndItemPickupPointUpdateForCombinedEdit(String requestId,
      boolean updateCategory, ProductDetailPageEditRequest productDetailPageEditRequest, String productSku,
      MandatoryRequestParam mandatoryRequestParam) throws Exception {
    EditItemResponse editItemResponse;
    EditProductDetailDTO editProductDetailDTO;
    validateProductEditRequest(productDetailPageEditRequest.getProductEditRequest());
    validateItemPickupPointUpdateRequest(productDetailPageEditRequest.getItemPickupPointUpdateRequest());
    editProductDetailDTO = productService.generateEditProductDetailDTO(updateCategory, productSku, requestId,
        productDetailPageEditRequest.getEditChangeType());
    log.info("Processing content update in combined edit for productSku : {} ", productSku);
    editProductDetailDTO =
        updateEditedProduct(requestId, updateCategory, productDetailPageEditRequest.getProductEditRequest(), productSku,
            editProductDetailDTO);
    log.info("Processing L5 update in combined edit for productSku : {}, with editProductDetailDTO : {} ", productSku,
        editProductDetailDTO);
    editItemResponse =
        updateItemPickupPoint(productDetailPageEditRequest.getItemPickupPointUpdateRequest(), mandatoryRequestParam,
            productSku, editProductDetailDTO);
    log.info("L5 update successfully completed for productSku : {} ", productSku);
    return editItemResponse;
  }

  private EditItemResponse processItemPickupPointUpdateForCombinedEdit(String requestId, boolean updateCategory,
      ProductDetailPageEditRequest productDetailPageEditRequest, String productSku,
      MandatoryRequestParam mandatoryRequestParam) throws Exception {
    EditProductDetailDTO editProductDetailDTO;
    EditItemResponse editItemResponse;
    validateItemPickupPointUpdateRequest(productDetailPageEditRequest.getItemPickupPointUpdateRequest());
    editProductDetailDTO = productService.generateEditProductDetailDTO(updateCategory, productSku, requestId,
        productDetailPageEditRequest.getEditChangeType());
    log.info("Processing L5 update for product : {}, with request : {} , editProductDetailDTO : {} ", productSku,
        productDetailPageEditRequest.getItemPickupPointUpdateRequest(), editProductDetailDTO);
    editItemResponse =
        updateItemPickupPoint(productDetailPageEditRequest.getItemPickupPointUpdateRequest(), mandatoryRequestParam,
            productSku, editProductDetailDTO);
    log.info("L5 update successfully completed for product : {} ", productSku);
    return editItemResponse;
  }

  private Long processContentUpdateForCombinedEdit(String requestId, boolean updateCategory,
      ProductDetailPageEditRequest productDetailPageEditRequest, String productSku,
      MandatoryRequestParam mandatoryRequestParam) throws Exception {
    EditProductDetailDTO editProductDetailDTO;
    validateProductEditRequest(productDetailPageEditRequest.getProductEditRequest());
    editProductDetailDTO = productService.generateEditProductDetailDTO(updateCategory, productSku, requestId,
        productDetailPageEditRequest.getEditChangeType());
    log.info("Processing content update for product : {}, with request : {} ", productSku,
        productDetailPageEditRequest.getProductEditRequest());
    //Content update
    editProductDetailDTO =
        updateEditedProduct(requestId, updateCategory, productDetailPageEditRequest.getProductEditRequest(), productSku,
            editProductDetailDTO);
    log.info("Content update successfully done for product : {}, with editProductDetailDTO : {} ", productSku,
        editProductDetailDTO);
    //Generate product score
    List<ProductAndItemsVO> productAndItemsVOList =
        productService.updateProductScoreOnMasterDataChange(mandatoryRequestParam.getStoreId(), StringUtils.EMPTY, true,
            productSku, editProductDetailDTO.isUpdateCategory(), true, editProductDetailDTO, null);
    log.info("Processing final save for content edit for product : {}, with editProductDetailDTO : {} ", productSku,
        editProductDetailDTO);
    //Final save
    productService.processFinalSaveAndPublishEventForContentEditOnly(editProductDetailDTO);
    log.info("Content update successfully completed for product : {} ", productSku);
    return productAndItemsVOList.get(0).getProduct().getVersion();
  }

  private EditItemResponse updateItemPickupPoint(ItemPickupPointUpdateRequest itemPickupPointUpdateRequest,
      MandatoryRequestParam mandatoryRequestParam, String productSku, EditProductDetailDTO editProductDetailDTO)
      throws Exception {
    EditItemResponse editItemResponse = null;
    log.info("Modify/add/delete item Pickup Points request : {} ", itemPickupPointUpdateRequest);
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        modelConverter.covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    editItemResponse =
        itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
            editProductDetailDTO);
    if (Objects.isNull(editItemResponse) || Objects.nonNull(editItemResponse.getApiErrorCode())) {
      log.error("Error while updating the edit info for productSku : {} with request : {} error code : {}",
          itemPickupPointUpdateRequest.getProductSku(), itemPickupPointUpdateRequest, editItemResponse);
    }
    return editItemResponse;
  }

  private EditProductDetailDTO updateEditedProduct(String requestId, boolean updateCategory,
      ProductEditRequest productEditRequest, String productSku, EditProductDetailDTO editProductDetailDTO)
      throws Exception {
    log.info("Update edited product with productRequest = {}", productEditRequest);
    Product product = null;
    EditProductDetailDTO result;
    if (Objects.nonNull(productEditRequest.getProductRequest())) {
      product = this.modelConverter.convertToProduct(productEditRequest.getProductRequest());
    }
    Map<String, ItemViewConfig> itemViewConfigMap = new HashMap<>();
    if (!CollectionUtils.isEmpty(productEditRequest.getItemViewConfigAndItemSkuListRequest())) {
      itemViewConfigMap =
          this.modelConverter.convertToItemViewConfigMap(productEditRequest.getItemViewConfigAndItemSkuListRequest());
    }
    return this.productService.updateEditedProduct(requestId, product, itemViewConfigMap,
        updateCategory, true, editProductDetailDTO, productEditRequest);
  }

  private static void validateProductDetailPageEditRequest(ProductDetailPageEditRequest productDetailPageEditRequest) {
    GdnPreconditions.checkArgument(Objects.nonNull(productDetailPageEditRequest),
        "ProductDetailPageEditRequest can not be null");
    GdnPreconditions.checkArgument(Objects.nonNull(productDetailPageEditRequest.getEditChangeType()),
        "EditChangeType can not be null");
  }

  private static void validateItemPickupPointUpdateRequest(ItemPickupPointUpdateRequest itemPickupPointUpdateRequest) {
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPointUpdateRequest),
        "ItemPickupPointUpdateRequest can not be null");
  }

  private static void validateProductEditRequest(ProductEditRequest productEditRequest) {
    GdnPreconditions.checkArgument(Objects.nonNull(productEditRequest), "ProductEditRequest can not be null");
  }
}
