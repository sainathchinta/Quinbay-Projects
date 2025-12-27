package com.gdn.x.product.outbound.helper;

import java.util.ArrayList;
import java.util.List;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuByItemSkuAndItemCodesResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;

/**
 * @author nitinmathew - created on 03/02/2020
 **/
public class ProductPricingResponseHelper {

  public static ActivePromoBundlingResponseVO toActivePromoBundlingResponseVo(
      GdnRestListResponse<PromoBundlingSkuDetailResponse> response, GdnMapper gdnMapper) {
    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOs = new ArrayList<>();
    response.getContent().forEach(promoBundlingSkuDetailResponse -> {
      PromoBundlingDetailResponseVO promoBundlingDetailResponseVO =
          gdnMapper.deepCopy(promoBundlingSkuDetailResponse, PromoBundlingDetailResponseVO.class);
      promoBundlingDetailResponseVOs.add(promoBundlingDetailResponseVO);
    });
    ActivePromoBundlingResponseVO activePromoBundlingResponseVO = new ActivePromoBundlingResponseVO();
    activePromoBundlingResponseVO.setPromoBundlingDetailResponseVOList(promoBundlingDetailResponseVOs);
    activePromoBundlingResponseVO.setTotalRecords((int) response.getPageMetaData().getTotalRecords());
    return activePromoBundlingResponseVO;
  }

  public static PromoBundlingByItemSkuAndItemCodesResponseVO toPromoBundlingByItemSkuAndItemCodesResponseVO(
      PromoBundlingSkuByItemSkuAndItemCodesResponse response, GdnMapper gdnMapper) {
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO =
        gdnMapper.deepCopy(response, PromoBundlingByItemSkuAndItemCodesResponseVO.class);
    responseVO.setPromoBundling(gdnMapper
        .deepCopy(response.getPromoBundlings().stream().findFirst().orElse(new PromoBundlingSkuDetailResponse()),
            PromoBundlingDetailResponseVO.class));
    return responseVO;
  }
}
