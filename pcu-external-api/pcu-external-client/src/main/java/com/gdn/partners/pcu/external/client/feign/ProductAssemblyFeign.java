package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.factory.ProductAssemblyFeignFallbackFactory;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.TransferRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBundleRecipeEditableResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(name = "productAssemblyFeign", url = "${service.product-assembly-service.endpoint}",
             fallbackFactory = ProductAssemblyFeignFallbackFactory.class)
public interface ProductAssemblyFeign {

  @RequestMapping(value = "/api/warehouse/getWarehouseCodeAndFulfillmentCenter", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<MasterWarehouseListWebResponse> getWarehouseCodeAndFulfillmentCenter(
      @RequestParam("page") String page, @RequestParam("limit") String limit);

  @RequestMapping(value = "/api/product-assembly/listing", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<RequestFormResponse> getListingResponse(@RequestParam("page") int page,
      @RequestParam("size") int size, AssemblyDisAssemblyListingRequest request);

  @RequestMapping(value = "/api/product-assembly/get-history", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<HistoryWebResponse> getHistory(@RequestParam("page") int page, @RequestParam("size") int size,
      @RequestParam("sortOrder") String sortOrder, @RequestParam("requestFormNumber") String requestFormNumber, @RequestParam("merchantCode") String merchantCode);

  @RequestMapping(value = "/api/item/getProductBundleRecipeEditableInfoByItemCodes", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProductBundleRecipeEditableResponse> getBundleRecipeEditableInfoByItemCodes(
      @RequestBody List<String> itemCodes);

  @RequestMapping(value = "/api/product-assembly/{type}/assembly-disassembly-request", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse assemblyDisassemblyRequest(@PathVariable("type") String type,
      @RequestBody SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest);

  @RequestMapping(value = "/api/product-assembly/transfer-request", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse transferRequest(@RequestBody TransferRequest transferRequest);

  @RequestMapping(value = "/api/product-assembly/retry-or-cancel", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse cancelOrRetryRequestForm(@RequestParam("requestFormNumber") String requestFormNumber,
      @RequestParam("type") String type, @RequestParam("merchantCode") String merchantCode, @RequestParam("requestId") String requestId);
}
