package com.gdn.partners.pcu.master.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.master.client.factory.XProductFeignFallBackFactory;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;

@FeignClient(name = "x-productFeign", url = "${service.x-product.endpoint}", fallbackFactory = XProductFeignFallBackFactory.class)
public interface XProductFeign {

  @RequestMapping(value = "/api/summary/searchBySalesCatalog", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductSummaryResponse> getListOfProductSummaryBySalesCatalog(
      @RequestParam("catalogCode") String catalogCode, @RequestParam("categoryCode") String categoryCode,
      @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/product-v2/getProductSkuListBySizeChartCode", method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductSkuSizeChartResponse> checkAnyProductsMappedToSizeChart(
      @RequestParam(value = "page", defaultValue = "0") int page,
      @RequestParam(value = "size", defaultValue = "1") int size,
      @RequestParam("sizeChartCode") String sizeChartCode);

}
