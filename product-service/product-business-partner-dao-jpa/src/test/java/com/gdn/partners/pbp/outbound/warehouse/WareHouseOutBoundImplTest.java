package com.gdn.partners.pbp.outbound.warehouse;

import java.util.Arrays;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.outbound.warehouse.feign.WareHouseFeign;
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import com.google.common.collect.ImmutableMap;

public class WareHouseOutBoundImplTest {

  private static final String ITEM_CODE = "item-code";

  @InjectMocks
  private WareHouseOutBoundBean wareHouseOutBound;

  @Mock
  private WareHouseFeign wareHouseFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(wareHouseFeign);
  }

  @Test
  public void createAndUpdateProductBundleTest() throws Exception {
    Response<CreateUpdateBOMRecipeResponse> response = new Response<>();
    response.setStatus(HttpStatus.OK.getReasonPhrase());
    Mockito.when(
        wareHouseFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any())).thenReturn(response);
    wareHouseOutBound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest());
    Mockito.verify(wareHouseFeign)
        .createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());
  }

  @Test
  public void createAndUpdateProductBundle_ExceptionTest() throws Exception {
    try {
      Response<CreateUpdateBOMRecipeResponse> response = new Response<>();
      response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase());
      Mockito.when(
          wareHouseFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any()))
          .thenReturn(response);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        wareHouseOutBound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest());
      });
    } finally {
      Mockito.verify(wareHouseFeign)
          .createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());
    }
  }

  @Test
  public void createAndUpdateProductBundleWrongResponseTest() throws Exception {
    Response<CreateUpdateBOMRecipeResponse> response = new Response<>();
    response.setStatus(HttpStatus.OK.getReasonPhrase());
    response.setErrors(ImmutableMap.of("message", Arrays.asList("CYCLIC BOM FOUND")));
    Mockito.when(
        wareHouseFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        wareHouseOutBound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest());
      });
    } finally {
      Mockito.verify(wareHouseFeign)
          .createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());
    }
  }
}
