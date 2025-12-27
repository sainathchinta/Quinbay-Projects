package com.gdn.mta.bulk.repository.generator;

import java.util.UUID;

import com.gdn.mta.bulk.feignConfig.PBPFeign;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;

public class GeneratorRepositoryTest {

  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private GeneratorRepositoryBean generatorRepositoryBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, GeneratorRepositoryTest.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, GeneratorRepositoryTest.DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<GenerateShippingWeightResponse> baseGenerateShippingWeightResponse =
        new GdnRestSingleResponse<GenerateShippingWeightResponse>(this.generateGenerateShippingWeightResponse(),
            GeneratorRepositoryTest.DEFAULT_REQUEST_ID);
    Mockito.when(
            this.pbpFeign.generateShippingWeight(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(baseGenerateShippingWeightResponse);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pbpFeign);
  }

  private GenerateShippingWeightResponse generateGenerateShippingWeightResponse() throws Exception {
    GenerateShippingWeightResponse generateShippingWeightResponse = new GenerateShippingWeightResponse();
    generateShippingWeightResponse.setShippingWeight(1D);
    return generateShippingWeightResponse;
  }

  private GenerateShippingWeightRequest generateGenerateShippingWeightRequest() throws Exception {
    GenerateShippingWeightRequest generateShippingWeightRequest = new GenerateShippingWeightRequest();
    return generateShippingWeightRequest;
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    this.generatorRepositoryBean.generateShippingWeight(this.generateGenerateShippingWeightRequest());
    Mockito.verify(this.pbpFeign)
        .generateShippingWeight(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(),
            Mockito.any(GenerateShippingWeightRequest.class));
  }

  @Test
  public void generateShippingWeightWithExceptionTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "");
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "");
    GdnRestSingleResponse<GenerateShippingWeightResponse> baseGenerateShippingWeightResponseError =
        new GdnRestSingleResponse<GenerateShippingWeightResponse>("Error Message", ErrorCategory.UNSPECIFIED.name(),
            false, null, GeneratorRepositoryTest.DEFAULT_REQUEST_ID);
    Mockito.when(
            this.pbpFeign.generateShippingWeight(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.any(GenerateShippingWeightRequest.class)))
        .thenReturn(baseGenerateShippingWeightResponseError);
    try {
      this.generatorRepositoryBean.generateShippingWeight(this.generateGenerateShippingWeightRequest());
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        Mockito.verify(this.pbpFeign)
            .generateShippingWeight(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(),
                Mockito.any(GenerateShippingWeightRequest.class));
      }
    }
  }

}
