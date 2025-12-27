package com.gdn.mta.bulk.service;

import java.util.Collections;
import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.feignConfig.CMSFeign;
import com.gdn.mta.bulk.request.ShortUrlMappingRequest;
import com.gdn.mta.bulk.request.UrlMapping;
import com.gdn.mta.bulk.response.GenerateShortUrlResponse;
import com.gdn.mta.bulk.response.Response;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CMSOutboundServiceImpl implements CMSOutboundService {

  @Autowired
  private CMSFeign cmsFeign;

  @Value("${cms.short.url.prefix}")
  private String cmsShortUrlPrefix;

  @Override
  public String generateShortUrl() {
    Response<GenerateShortUrlResponse> generateShortUrlResponseResponse = cmsFeign.generateShortUrl();
    if (Objects.isNull(generateShortUrlResponseResponse)
        || !Constant.RESPONSE_OK.equals(generateShortUrlResponseResponse.getStatus())
        || !Constant.RESPONSE_200.equals(generateShortUrlResponseResponse.getCode())
        || Objects.isNull(generateShortUrlResponseResponse.getData())
        || StringUtils.isBlank(generateShortUrlResponseResponse.getData().getShortUrl())) {
      log.error("Failed to generate short URL, response: {}", generateShortUrlResponseResponse);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return generateShortUrlResponseResponse.getData().getShortUrl().split(cmsShortUrlPrefix) [1];
  }
  @Override
  public Boolean mapLongAndShortUrl(String shortUrl, String longUrl) {
    String uniqueRequestName =
        new StringBuilder().append(Constant.CLIENT_ID).append(Constant.DASH).append(UUID.randomUUID()).toString();
    ShortUrlMappingRequest shortUrlMappingRequest =
        ShortUrlMappingRequest.builder().longUrl(longUrl).name(uniqueRequestName).active(true)
            .urlMappingUtmList(Collections.singletonList(UrlMapping.builder().shortUrl(shortUrl).build())).build();
    Response<Boolean> booleanResponse = cmsFeign.mapShortAndLongUrl(Constant.CLIENT_ID, shortUrlMappingRequest);
    if (Objects.isNull(booleanResponse)
        || !Constant.RESPONSE_OK.equals(booleanResponse.getStatus())
        || !Constant.RESPONSE_200.equals(booleanResponse.getCode())
        || Boolean.FALSE.equals(booleanResponse.getData())) {
      log.error("Failed to generate short URL, response: {}, unique request name: {}", booleanResponse,
          uniqueRequestName);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage());
    }
    return booleanResponse.getData();
  }
}
