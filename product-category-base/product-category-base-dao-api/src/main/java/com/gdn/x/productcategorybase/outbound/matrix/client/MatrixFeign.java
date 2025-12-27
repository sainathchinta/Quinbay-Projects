package com.gdn.x.productcategorybase.outbound.matrix.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;

import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionRequest;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;
import feign.Headers;
import feign.RequestLine;

@FeignClient(name = "matrixFeign", url = "${service.matrix.endpoint}")
public interface MatrixFeign {

  @RequestLine("POST /extractAttributes")
  @Headers({"Content-Type: application/json", "Accept: application/json"})

  @PostMapping(value = "/extractAttributes", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  SingleBaseResponse<MatrixAttributeExtractionResponse> extractProductAttributesByTextDetails(
      MatrixAttributeExtractionRequest matrixAttributeExtractionRequest);

}
