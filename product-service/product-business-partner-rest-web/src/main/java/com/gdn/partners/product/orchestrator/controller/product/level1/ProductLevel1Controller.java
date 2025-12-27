package com.gdn.partners.product.orchestrator.controller.product.level1;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterRequest;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterResponse;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import com.gdn.partners.product.orchestrator.service.product.level1.ProductLevel1Service;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController(value = "productLevel1ControllerV2")
@RequestMapping(value = ProductLevel1ControllerPath.BASE_PATH)
@Tag(name = "ProductLevel1Controller", description = "Product Level 1 Controller")
public class ProductLevel1Controller {

  @Autowired
  private ProductLevel1Service productLevel1Service;

  @RequestMapping(value = ProductLevel1ControllerPath.FILTER, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Product filter api", description = "Product filter api")
  public GdnRestListResponse<ProductLevel1FilterResponse> filter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody ProductLevel1FilterRequest request)
      throws Exception {
    ProductLevel1Filter filter = ProductLevel1Filter.builder().build();
    BeanUtils.copyProperties(request, filter, "sortDirection");
    if (!Objects.isNull(request.getSortDirection())) {
      filter.setSortDirection(Sort.Direction.valueOf(request.getSortDirection()));
    }
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductLevel1FilterResponse> productLevel1s = this.productLevel1Service.findByFilter(filter, pageable);
    return new GdnRestListResponse<>(productLevel1s.getContent(),
        new PageMetaData(size, page, productLevel1s.getTotalElements()), requestId);
  }

}