package com.gdn.x.product.outbound.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.exception.ProductPricingException;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.outbound.api.ProductPricingOutbound;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.outbound.api.feign.PromotionFeign;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.promotion.enums.PromoBundlingType;
import com.gdn.x.promotion.rest.web.model.dto.request.AdjustmentProductSkuRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.PromoBundlingDetail;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingByItemSkuAndItemCodesResponse;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;

@Service
public class PromotionOutboundImpl implements PromotionOutbound {

  private static final Logger LOGGER = LoggerFactory.getLogger(PromotionOutboundImpl.class);
  private static final String COMBO = "COMBO";
  private static final String X_PRODUCT_CLIENT_ID = "x-product";
  private static final String X_PRODUCT_CHANNEL_ID = "x-product";
  private static final String X_PRODUCT_RETRY_REQUEST = "x-product-retry";

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private PromotionFeign promotionFeign;

  @Autowired
  protected GdnMapper gdnMapper;

  @Value("${promo.bundling.product.pricing.route}")
  private boolean promoBundlingProductPricingRoute;

  @Override
  public List<AdjustmentProductResponse> getAdjustmentProduct(String requestId, String username,
      List<String> itemSkus) {
    List<AdjustmentProductResponse> result = new ArrayList<AdjustmentProductResponse>();
    if (itemSkus != null && !itemSkus.isEmpty()) {
      try {
        result =
            this.promotionFeign.getPromosBySku(Constants.DEFAULT_STORE_ID, X_PRODUCT_CHANNEL_ID, X_PRODUCT_CLIENT_ID,
                requestId, username, new AdjustmentProductSkuRequest(itemSkus, null)).getContent();
      } catch (Exception e) {
        PromotionOutboundImpl.LOGGER.warn("failed on getAdjustmentProduct {}", itemSkus, e);
      }
    }
    return result;
  }

  public List<AdjustmentProductChangeResponseVO> getAdjustmentProductBySkuAndPickupPointCode(
    String storeId, List<ItemPickupPointRequest> itemRequests) {
    PromotionOutboundImpl.LOGGER.info(
      "Calling promotion to fetch Product Adjustment details for request : {} ", itemRequests);
    GdnRestListResponse<AdjustmentProductChangeResponseVO>
      response =
      promotionFeign.getAdjustmentProductBySkuAndPickupPointCodeList(storeId, X_PRODUCT_CHANNEL_ID,
        X_PRODUCT_CLIENT_ID, X_PRODUCT_RETRY_REQUEST, Constants.DEFAULT_USERNAME, itemRequests);
    if (!response.isSuccess()) {
      PromotionOutboundImpl.LOGGER.error(
        "Failed to Fetch Adjustment details from Promotion for request : {}, error : ",
        itemRequests, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        "[" + response.getErrorCode() + "]" + response.getErrorMessage());
    }
    PromotionOutboundImpl.LOGGER.info("Product Adjustment detail Response From promotion was : {}",
      response.getContent());
    return response.getContent();
  }


  @Override
  public ActivePromoBundlingResponseVO getActiveCombosByItemCodes(MandatoryRequestParam param,
      int page, int size, String sortBy, String sortType, Set<String> itemCodes) {
    try {
      return productPricingOutbound.findActiveByPromoBundlingTypeAndItemCodes(param, page, size, sortBy, sortType,
          PromoBundlingType.COMBO.name(), itemCodes);
    } catch (ProductPricingException e) {
      PromotionOutboundImpl.LOGGER.warn("failed on getActiveCombosByItemCodes from product pricing {}", itemCodes, e);
    } catch (Exception e) {
      PromotionOutboundImpl.LOGGER.warn("failed on getActiveCombosByItemCodes {}", itemCodes, e);
    }
    return new ActivePromoBundlingResponseVO();
  }

  @Override
  public ActivePromoBundlingResponseVO getActiveByPromoBundlingTypeAndItemSkus(
      MandatoryRequestParam param, PromoBundlingType promoBundlingType, Set<String> itemSkus) {
      try{
          return productPricingOutbound
              .findActiveByPromoBundlingTypeAndItemSkus(param, promoBundlingType.name(), itemSkus);
      } catch (ProductPricingException e){
        PromotionOutboundImpl.LOGGER.warn("failed on getActiveByPromoBundlingTypeAndItemSkus from product pricing {}",
            itemSkus, e);
      } catch (Exception e){
        PromotionOutboundImpl.LOGGER.warn("failed on getActiveByPromoBundlingTypeAndItemSkus {}",
            itemSkus, e);
      }
      return new ActivePromoBundlingResponseVO();
  }

  private ActivePromoBundlingResponseVO convertPromoBundlingDetailResponseToActivePromoBundlingVo(
      GdnRestListResponse<PromoBundlingDetailResponse> promoBundlingDetailResponses) {
    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOs = new ArrayList<>();
    promoBundlingDetailResponses.getContent().stream()
        .forEach(promoBundlingDetailResponse -> {
          PromoBundlingDetailResponseVO promoBundlingDetailResponseVO =
              gdnMapper.deepCopy(promoBundlingDetailResponse, PromoBundlingDetailResponseVO.class);
          promoBundlingDetailResponseVOs.add(promoBundlingDetailResponseVO);
        });

    ActivePromoBundlingResponseVO activePromoBundlingResponseVO =
        new ActivePromoBundlingResponseVO();
    activePromoBundlingResponseVO
        .setPromoBundlingDetailResponseVOList(promoBundlingDetailResponseVOs);
    activePromoBundlingResponseVO
        .setTotalRecords((int) promoBundlingDetailResponses.getPageMetaData().getTotalRecords());

    return activePromoBundlingResponseVO;
  }

  @Override
  public List<PromoBundlingDetailResponseVO> getPromoBundlingDetailByPromoBundlingIds(
      MandatoryRequestParam mandatoryRequestParam, Set<String> promoBundlingIds, Set<String> itemSkus) {
    try {
        return productPricingOutbound.findActivePromoBundlingByIds(mandatoryRequestParam, promoBundlingIds, itemSkus);
    } catch (ProductPricingException e) {
      PromotionOutboundImpl.LOGGER.error(
          "failed to getPromoBundlingDetailByPromoBundlingIds from product pricing, {}", promoBundlingIds, e);
    } catch (Exception e) {
      PromotionOutboundImpl.LOGGER.error(
          "failed to getPromoBundlingDetailByPromoBundlingIds, {}", promoBundlingIds, e);
    }
    return Collections.emptyList();
  }

  @Override
  public PromoBundlingByItemSkuAndItemCodesResponseVO getActiveAndPromoBundlingTotalByPromoBundlingType(
      MandatoryRequestParam param, String promoBundlingType, String itemSku, Set<String> itemCodes,
      boolean includeNewBundlings) {
    try {
        return productPricingOutbound
            .getActiveAndPromoBundlingTotalByPromoBundlingType(param, promoBundlingType, itemSku, itemCodes,
                includeNewBundlings);
    } catch (ProductPricingException e) {
      PromotionOutboundImpl.LOGGER
          .error(
              "failed on getActiveAndPromoBundlingTotalByPromoBundlingType from product pricing with "
                  + "itemCodes : {}, itemSku : {} and promoBundlingType : {}",
              itemCodes, itemSku, promoBundlingType, e);
    } catch (Exception e) {
      PromotionOutboundImpl.LOGGER
          .error(
              "failed on getActiveAndPromoBundlingTotalByPromoBundlingType with "
                  + "itemCodes : {}, itemSku : {} and promoBundlingType : {}",
              itemCodes, itemSku, promoBundlingType, e);
    }
    return new PromoBundlingByItemSkuAndItemCodesResponseVO();
  }


  private PromoBundlingByItemSkuAndItemCodesResponseVO toPromoBundlingByItemSkuAndItemCodesResponseVO(
      PromoBundlingByItemSkuAndItemCodesResponse response) throws Exception{
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO = gdnMapper.deepCopy(response,
        PromoBundlingByItemSkuAndItemCodesResponseVO.class);

    PromoBundlingDetail promoBundlingDetail =
        response.getPromoBundling().stream().findFirst().orElse(new PromoBundlingDetail());

    responseVO.setPromoBundling(
        gdnMapper.deepCopy(promoBundlingDetail, PromoBundlingDetailResponseVO.class));
    responseVO.setTotalWholesaleRule(response.getTotalWholesaleRule());
    responseVO.setTotalComboRule(response.getTotalComboRule());

    return responseVO;
  }
}
