package com.gdn.partners.pcu.external.client.feign;

import com.gdn.partners.pcu.external.client.factory.VideoFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.web.model.request.VideoConfigurationRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoHashingRequest;
import com.gdn.partners.pcu.external.web.model.request.VideoSignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.VideoSignedUrlResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.Map;

@FeignClient(name = "videoFeign", url = "${service.video.endpoint}", fallbackFactory =
    VideoFeignFallbackFactory.class)
public interface VideoFeign {

  @PostMapping(value = "/api/v1/configurations/generate-signed-url", consumes = MediaType.APPLICATION_JSON_VALUE
      , produces = MediaType.APPLICATION_JSON_VALUE)
  Response<VideoSignedUrlResponse> generateSignedUrl(
      @RequestBody VideoSignedUrlRequest videoSignedUrlRequest);


  @PostMapping(value = "/api/v1/configurations/get-configurations", consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  Response<Map<String, Object>> fetchConfiguration(@RequestBody VideoConfigurationRequest request);

  @PostMapping(value = "/api/v1/videos/{videoId}/retry-compression",
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  Response<Void> retryVideoCompression(@PathVariable String videoId);

  @PostMapping(value = "/api/v1/videos/generateFingerprint",
    consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  Response<Void> generateFingerPrint(@RequestBody VideoHashingRequest request);

} 