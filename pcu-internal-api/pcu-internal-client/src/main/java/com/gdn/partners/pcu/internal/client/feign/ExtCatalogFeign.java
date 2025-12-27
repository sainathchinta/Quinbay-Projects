package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.ext.catalog.rest.web.model.response.PristineCategoryMapResponse;
import com.gdn.partners.pcu.internal.client.factory.ExtCatalogFallbackFactory;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;

@FeignClient(name = "extCatalogFeign", url = "${service.extCatalog.endpoint}",
    fallbackFactory = ExtCatalogFallbackFactory.class)
public interface ExtCatalogFeign {

  @RequestMapping(value = "/product-screening/get-categories", method = RequestMethod.GET)
  GdnRestSingleResponse<PristineCategoryMapResponse> getSupportedBlibliCategoriesByPristine();

  @RequestMapping(value = "/product-screening/get-product", method = RequestMethod.GET)
  GdnRestListResponse<ProductCodeResponse> getPCBProductCodes(@RequestParam("productCode") String productCode,
      @RequestParam("category") String category, @RequestParam("page") int page, @RequestParam("size") int size);

}
