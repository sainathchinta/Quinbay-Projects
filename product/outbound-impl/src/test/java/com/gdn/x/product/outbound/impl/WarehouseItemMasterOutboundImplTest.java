package com.gdn.x.product.outbound.impl;

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
import com.gdn.warehouse.itemmaster.webmodel.response.CreateUpdateBOMRecipeResponse;
import com.gdn.x.product.outbound.api.feign.WarehouseItemMasterFeign;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.google.common.collect.ImmutableMap;

public class WarehouseItemMasterOutboundImplTest {

  private static final String ITEM_CODE = "item-code";

  @InjectMocks
  private WarehouseItemMasterOutboundImpl warehouseItemMasterOutbound;

  @Mock
  private WarehouseItemMasterFeign warehouseItemMasterFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(warehouseItemMasterFeign);
  }

  @Test
  public void createAndUpdateProductBundleTest() throws Exception {
    Response<CreateUpdateBOMRecipeResponse> response = new Response<>();
    response.setStatus(HttpStatus.OK.getReasonPhrase());
    Mockito.when(
        warehouseItemMasterFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any())).thenReturn(response);
    warehouseItemMasterOutbound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest());
    Mockito.verify(warehouseItemMasterFeign)
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
              warehouseItemMasterFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
                  Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
                  Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
                  Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
                  Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any()))
          .thenReturn(response);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> warehouseItemMasterOutbound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest()));
    } finally {
      Mockito.verify(warehouseItemMasterFeign)
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
        warehouseItemMasterFeign.createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  warehouseItemMasterOutbound.createAndUpdateProductBundle(new CreateUpdateBillOfMaterialRecipeCommandRequest()));
    } finally {
      Mockito.verify(warehouseItemMasterFeign)
          .createUpdateProductBundle(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
              Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.any());
    }
  }
}
