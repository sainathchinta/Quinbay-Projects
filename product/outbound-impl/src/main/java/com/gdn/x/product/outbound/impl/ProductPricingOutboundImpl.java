package com.gdn.x.product.outbound.impl;

import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.ItemInfoRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.PromoBundlingRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuByItemSkuAndItemCodesResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.exception.ProductPricingException;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.outbound.api.feign.ProductPricingFeign;
import com.gdn.x.product.outbound.api.ProductPricingOutbound;
import com.gdn.x.product.outbound.helper.ProductPricingResponseHelper;
import com.gdn.x.product.outbound.helper.ResponseHelper;

/**
 * @author nitinmathew - created on 03/02/2020
 **/
@Service
public class ProductPricingOutboundImpl implements ProductPricingOutbound {

  @Autowired
  private ProductPricingFeign productPricingFeign;

  @Autowired
  protected GdnMapper gdnMapper;

  @Override
  public ActivePromoBundlingResponseVO findActiveByPromoBundlingTypeAndItemSkus(MandatoryRequestParam param,
      String promoBundlingType, Set<String> itemSkus) throws Exception {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response = productPricingFeign
        .getActiveBundlingPromos(param.getStoreId(), param.getChannelId(), param.getClientId(), param.getRequestId(),
            param.getUsername(),
            PromoBundlingRequest.builder().promoBundlingType(promoBundlingType).itemSkus(itemSkus).build());
    if (Objects.isNull(response)) {
      throw new ProductPricingException("failed to findActiveByPromoBundlingTypeAndItemSkus");
    } else if (!ResponseHelper.isValid(response)) {
      throw new ProductPricingException(response.getErrorMessage());
    }
    return ProductPricingResponseHelper.toActivePromoBundlingResponseVo(response, gdnMapper);
  }

  @Override
  public List<PromoBundlingSkuDetailResponse> findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(
      MandatoryRequestParam param, String promoBundlingType, Set<ItemInfoRequest> itemInfoRequests) {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response =
        productPricingFeign.getActiveBundlingPromos(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(),
            PromoBundlingRequest.builder().promoBundlingType(promoBundlingType).itemInfoRequests(itemInfoRequests)
                .build());
    if (Objects.isNull(response)) {
      throw new ProductPricingException("failed to findActiveByPromoBundlingTypeAndItemSkus");
    } else if (!ResponseHelper.isValid(response)) {
      throw new ProductPricingException(response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ActivePromoBundlingResponseVO findActiveByPromoBundlingTypeAndItemCodes(MandatoryRequestParam param, int page,
      int size, String sortBy, String sortType, String promoBundlingType, Set<String> itemCodes) throws Exception {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response = productPricingFeign
        .getActiveBundlingPromosByTypeAndItemCodes(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), page, size, sortBy, sortType,
            PromoBundlingRequest.builder().promoBundlingType(promoBundlingType).itemCodes(itemCodes).build());
    if (Objects.isNull(response)) {
      throw new ProductPricingException("failed to findActiveByPromoBundlingTypeAndItemCodes");
    } else if (!ResponseHelper.isValid(response)) {
      throw new ProductPricingException(response.getErrorMessage());
    }
    return ProductPricingResponseHelper.toActivePromoBundlingResponseVo(response, gdnMapper);
  }

  @Override
  public List<PromoBundlingDetailResponseVO> findActivePromoBundlingByIds(MandatoryRequestParam param,
      Set<String> promoBundlingIds, Set<String> itemSkus) {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response = productPricingFeign
        .getBundlingPromosByIds(param.getStoreId(), param.getChannelId(), param.getClientId(), param.getRequestId(),
            param.getUsername(),
            PromoBundlingRequest.builder().promoBundlingIds(promoBundlingIds).itemSkus(itemSkus).build());
    if (Objects.isNull(response)) {
      throw new ProductPricingException("failed to getActivePromoBundlingByPromoBundlingIds");
    } else if (!ResponseHelper.isValid(response)) {
      throw new ProductPricingException(response.getErrorMessage());
    }
    return ProductPricingResponseHelper.toActivePromoBundlingResponseVo(response, gdnMapper)
        .getPromoBundlingDetailResponseVOList();
  }

  @Override
  public PromoBundlingByItemSkuAndItemCodesResponseVO getActiveAndPromoBundlingTotalByPromoBundlingType(
      MandatoryRequestParam param, String promoBundlingType, String itemSku, Set<String> itemCodes,
      boolean includeNewBundlings) {
    GdnRestSingleResponse<PromoBundlingSkuByItemSkuAndItemCodesResponse> response = productPricingFeign
        .getActiveAndPromoBundlingTotalByItemCodes(param.getStoreId(), param.getChannelId(), param.getClientId(),
            param.getRequestId(), param.getUsername(), itemSku,
            PromoBundlingRequest.builder().promoBundlingType(promoBundlingType).itemCodes(itemCodes)
                .includeNewBundlings(includeNewBundlings).build());
    if (Objects.isNull(response)) {
      throw new ProductPricingException("failed to getActiveAndPromoBundlingTotalByPromoBundlingType");
    } else if (!ResponseHelper.isValid(response)) {
      throw new ProductPricingException(response.getErrorMessage());
    }
    return ProductPricingResponseHelper.toPromoBundlingByItemSkuAndItemCodesResponseVO(response.getValue(), gdnMapper);
  }
}
