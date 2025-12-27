package com.gdn.partners.pcu.external.client.feign;

import com.gdn.partners.pcu.external.client.factory.SellerLogisticsFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.seller.logistics.web.model.request.UploadExcelRequest;
import com.gdn.seller.logistics.web.model.response.DownloadSkuTemplateResponse;
import com.gdn.seller.logistics.web.model.response.GetSellerLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateResponse;
import com.gdn.seller.logistics.web.model.response.UploadExcelSkuUpdateStatusResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(name = "sellerLogisticsFeign", url = "${service.seller.logistics.host}",
             fallbackFactory = SellerLogisticsFeignFallbackFactory.class)
public interface SellerLogisticsFeign {

  @RequestMapping(value = "/api/seller/logistics/get", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  Response<List<GetSellerLogisticProductResponse>> getSellerLogisticProducts(
      @RequestParam("merchantCode") String merchantCode,
      @RequestParam("merchantDeliveryType") String merchantDeliveryType);


  @RequestMapping(value = "/api/sku/logistics/downloadTemplate", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  Response<DownloadSkuTemplateResponse> getTemplateData(
      @RequestParam("merchantCode") String merchantCode,
      @RequestParam("merchantDeliveryType") String merchantDeliveryType);

  @RequestMapping(value = "/api/uploadExcel/sku/update", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  Response<UploadExcelSkuUpdateResponse> excelUpload(@RequestBody UploadExcelRequest requestBody,
      @RequestParam("merchantCode") String merchantCode,
      @RequestParam("merchantDeliveryType") String merchantDeliveryType);

  @RequestMapping(value = "/api/uploadExcel/sku/status", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  Response<UploadExcelSkuUpdateStatusResponse> excelUploadStatus(
      @RequestParam("merchantCode") String merchantCode);
}
