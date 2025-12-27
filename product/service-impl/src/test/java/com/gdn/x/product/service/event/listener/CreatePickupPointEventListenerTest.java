package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerAddPickupPoint;
import com.gdn.x.businesspartner.domain.event.model.PickupPointVO;
import com.gdn.x.product.domain.event.model.BusinessPartnerAddPickupPointEventModel;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.service.api.PickupPointService;

public class CreatePickupPointEventListenerTest {

  @InjectMocks
  private CreatePickupPointEventListener listener;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private ObjectMapper objectMapper;

  private BusinessPartnerAddPickupPointEventModel businessPartnerAddPickupPoint;
  private List<PickupPointVOEventModel> pickupPointVOS;
  private PickupPointVOEventModel pickupPointVO;
  private PickupPoint pickupPoint;


  private static final String STORE_ID = "10001";
  private static final String USERNAME = "system";
  private static final String BP_CODE = "BP-code";
  private static final String PICKUP_POINT_CODE = "PP-code";
  private static final String MESSAGE = "message";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    pickupPointVOS = new ArrayList<>();
    pickupPointVO = new PickupPointVOEventModel();
    pickupPointVO.setCode(PICKUP_POINT_CODE);
    pickupPointVO.setCncActivated(false);

    pickupPointVOS.add(pickupPointVO);

    businessPartnerAddPickupPoint = new BusinessPartnerAddPickupPointEventModel();
    businessPartnerAddPickupPoint.setAddPickupPoints(pickupPointVOS);
    businessPartnerAddPickupPoint.setBusinessPartnerCode(BP_CODE);

    pickupPoint = new PickupPoint();
    pickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPoint.setCncActivated(false);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pickupPointService);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void addPickupPoint_success() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, BusinessPartnerAddPickupPointEventModel.class)).thenReturn(
      businessPartnerAddPickupPoint);
    listener.onDomainEventConsumed(MESSAGE);
    verify(pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerAddPickupPointEventModel.class);
  }

  @Test
  public void addPickupPoint_exception() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, BusinessPartnerAddPickupPointEventModel.class)).thenReturn(
      businessPartnerAddPickupPoint);
    doThrow(new ApplicationRuntimeException()).when(pickupPointService)
        .upsertPickupPoint(
            STORE_ID, pickupPoint, USERNAME);
    listener.onDomainEventConsumed(MESSAGE);
    verify(pickupPointService).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.objectMapper).readValue(MESSAGE, BusinessPartnerAddPickupPointEventModel.class);
  }
}
