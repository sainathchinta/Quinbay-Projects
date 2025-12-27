package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.feignConfig.CMSFeign;
import com.gdn.mta.bulk.request.ShortUrlMappingRequest;
import com.gdn.mta.bulk.response.GenerateShortUrlResponse;
import com.gdn.mta.bulk.response.Response;
import com.gdn.partners.bulk.util.Constant;

public class CMSOutboundServiceTest {

  private String shortUrl = "blib.li/suffix";
  private String urlSuffix = "suffix";

  private Response<GenerateShortUrlResponse> generateShortUrlResponse;
  private Response<Boolean> mapShortAndLongUrlResponse = new Response<>();

  @Mock
  private CMSFeign cmsFeign;

  @InjectMocks
  private CMSOutboundServiceImpl cmsOutboundService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    generateShortUrlResponse =
        Response.<GenerateShortUrlResponse>builder().code(Constant.RESPONSE_200).status(Constant.RESPONSE_OK)
            .data(GenerateShortUrlResponse.builder().shortUrl(shortUrl).build()).build();
    ReflectionTestUtils.setField(cmsOutboundService, "cmsShortUrlPrefix", "blib.li/");
    mapShortAndLongUrlResponse.setCode(Constant.RESPONSE_200);
    mapShortAndLongUrlResponse.setStatus(Constant.RESPONSE_OK);
    mapShortAndLongUrlResponse.setData(true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(cmsFeign);
  }

  @Test
  public void generateShortUrlSuccessTest() {
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(generateShortUrlResponse);
    String response = cmsOutboundService.generateShortUrl();
    Mockito.verify(cmsFeign).generateShortUrl();
    Assertions.assertEquals(urlSuffix, response);
  }

  @Test
  public void generateShortUrl_statusNotOkTest() {
    generateShortUrlResponse.setStatus(null);
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(generateShortUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> cmsOutboundService.generateShortUrl());
    } finally {
      Mockito.verify(cmsFeign).generateShortUrl();
    }
  }

  @Test
  public void generateShortUrl_codeNot200Test() {
    generateShortUrlResponse.setCode(null);
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(generateShortUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> cmsOutboundService.generateShortUrl());
    } finally {
      Mockito.verify(cmsFeign).generateShortUrl();
    }
  }

  @Test
  public void generateShortUrl_feignResponseNullTest() {
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> cmsOutboundService.generateShortUrl());
    } finally {
      Mockito.verify(cmsFeign).generateShortUrl();
    }
  }

  @Test
  public void generateShortUrl_dataNullTest() {
    generateShortUrlResponse.setData(null);
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(generateShortUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> cmsOutboundService.generateShortUrl());
    } finally {
      Mockito.verify(cmsFeign).generateShortUrl();
    }
  }

  @Test
  public void generateShortUrl_stringBlankTest() {
    generateShortUrlResponse.getData().setShortUrl(StringUtils.EMPTY);
    Mockito.when(cmsFeign.generateShortUrl()).thenReturn(generateShortUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> cmsOutboundService.generateShortUrl());
    } finally {
      Mockito.verify(cmsFeign).generateShortUrl();
    }
  }

  @Test
  public void mapLongAndShortUrlTest() {
    Mockito.when(cmsFeign.mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class)))
        .thenReturn(mapShortAndLongUrlResponse);
    Boolean response = cmsOutboundService.mapLongAndShortUrl(shortUrl, shortUrl);
    Mockito.verify(cmsFeign).mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class));
    Assertions.assertTrue(response);
  }

  @Test
  public void mapLongAndShortUrl_responseNot200Test() {
    mapShortAndLongUrlResponse.setCode(null);
    Mockito.when(cmsFeign.mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class)))
        .thenReturn(mapShortAndLongUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> cmsOutboundService.mapLongAndShortUrl(shortUrl, shortUrl));
    } finally {
      Mockito.verify(cmsFeign).mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class));

    }
  }

  @Test
  public void mapLongAndShortUrl_responseNotOkTest() {
    mapShortAndLongUrlResponse.setStatus(null);
    Mockito.when(cmsFeign.mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class)))
        .thenReturn(mapShortAndLongUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> cmsOutboundService.mapLongAndShortUrl(shortUrl, shortUrl));
    } finally {
      Mockito.verify(cmsFeign).mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class));

    }
  }

  @Test
  public void mapLongAndShortUrl_dataFalseTest() {
    mapShortAndLongUrlResponse.setData(false);
    Mockito.when(cmsFeign.mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class)))
        .thenReturn(mapShortAndLongUrlResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> cmsOutboundService.mapLongAndShortUrl(shortUrl, shortUrl));
    } finally {
      Mockito.verify(cmsFeign).mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class));

    }
  }

  @Test
  public void mapLongAndShortUrl_nullResponseTest() {
    Mockito.when(cmsFeign.mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class)))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> cmsOutboundService.mapLongAndShortUrl(shortUrl, shortUrl));
    } finally {
      Mockito.verify(cmsFeign).mapShortAndLongUrl(eq(Constant.CLIENT_ID), Mockito.any(ShortUrlMappingRequest.class));

    }
  }
}
