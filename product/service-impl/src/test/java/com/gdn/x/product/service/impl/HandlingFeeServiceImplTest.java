package com.gdn.x.product.service.impl;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.service.api.SystemParameterService;

public class HandlingFeeServiceImplTest {

  private static final int QUANTITY = 2;
  private static final String ORDER_ID = "123";
  private static final String ORDER_ID1 = "1234";
  private static final String ORDER_ID2 = "13456";
  private static final String BLANK = "";
  private static final String DESCRIPTION = "description";
  private static final String VALUE_FORMAT1 = "1:15000:12345,123;0:10000:1234,12347;0:12500: ";
  private static final String STORE_ID = "store-id";

  @InjectMocks
  private HandlingFeeServiceImpl handlingFeeServiceImpl;

  @Mock
  private SystemParameterService systemParameterService;

  private SystemParameter systemParameter;

  private List<HandlingFeeRequest> handlingFeeRequestList1;
  private List<HandlingFeeRequest> handlingFeeRequestList2;
  private List<HandlingFeeRequest> handlingFeeRequestList3;

  @Test
  public void calculateHandlingFeeNotFoundStatusTest() {

    HandlingFeeResponse response =
        this.handlingFeeServiceImpl.calculateHandlingFee(HandlingFeeServiceImplTest.STORE_ID,
            this.handlingFeeRequestList3);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE);

    assertThat(response, notNullValue());
    assertThat(response.getTotalHandlingFee(), equalTo(new BigDecimal(0)));
  }

  @Test
  public void calculateHandlingFeeNullHandlingFeeListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.handlingFeeServiceImpl.calculateHandlingFee(null, this.handlingFeeRequestList3));
  }

  @Test
  public void calculateHandlingFeeNullStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.handlingFeeServiceImpl.calculateHandlingFee(HandlingFeeServiceImplTest.STORE_ID, null));
  }

  @Test
  public void calculateHandlingFeeWith0StatusTest() {

    HandlingFeeResponse response =
        this.handlingFeeServiceImpl.calculateHandlingFee(HandlingFeeServiceImplTest.STORE_ID,
            this.handlingFeeRequestList2);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE);

    assertThat(response, notNullValue());
    assertThat(response.getTotalHandlingFee(), equalTo(new BigDecimal(10000)));
  }

  @Test
  public void calculateHandlingFeeWith1StatusTest() {

    HandlingFeeResponse response =
        this.handlingFeeServiceImpl.calculateHandlingFee(HandlingFeeServiceImplTest.STORE_ID,
            this.handlingFeeRequestList1);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE);

    assertThat(response, notNullValue());
    assertThat(response.getTotalHandlingFee(), equalTo(new BigDecimal(30000)));
  }

  @Test
  public void getAllSettingOfHandlingFeeTest() {
    SystemParameter response =
        this.handlingFeeServiceImpl.getAllSettingOfHandlingFee(HandlingFeeServiceImplTest.STORE_ID);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(
        HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE);

    assertThat(response, notNullValue());
    assertThat(response.getVariable(), equalTo(SystemParameterNames.HANDLING_FEE));
    assertThat(response, notNullValue());
  }

  @Test
  public void getAllSettingOfHandlingFeeTestWithNullStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.handlingFeeServiceImpl.getAllSettingOfHandlingFee(HandlingFeeServiceImplTest.BLANK));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.systemParameter =
        new SystemParameter(HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE,
            HandlingFeeServiceImplTest.VALUE_FORMAT1, HandlingFeeServiceImplTest.DESCRIPTION);

    when(
        this.systemParameterService.findValueByStoreIdAndVariable(
            HandlingFeeServiceImplTest.STORE_ID, SystemParameterNames.HANDLING_FEE)).thenReturn(
        this.systemParameter);

    // case 1
    this.handlingFeeRequestList1 = new ArrayList<HandlingFeeRequest>();
    this.handlingFeeRequestList1.add(new HandlingFeeRequest(HandlingFeeServiceImplTest.ORDER_ID,
        HandlingFeeServiceImplTest.QUANTITY));

    // case 2
    this.handlingFeeRequestList2 = new ArrayList<HandlingFeeRequest>();
    this.handlingFeeRequestList2.add(new HandlingFeeRequest(HandlingFeeServiceImplTest.ORDER_ID1,
        HandlingFeeServiceImplTest.QUANTITY));

    // case 3
    this.handlingFeeRequestList3 = new ArrayList<HandlingFeeRequest>();
    this.handlingFeeRequestList3.add(new HandlingFeeRequest(HandlingFeeServiceImplTest.ORDER_ID2,
        HandlingFeeServiceImplTest.QUANTITY));

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.systemParameterService);
  }

  @Test
  public void updateAllSettingOfHandlingFeeTest() {
    this.handlingFeeServiceImpl.updateAllSettingOfHandlingFee(this.systemParameter);

    verify(this.systemParameterService).update(this.systemParameter);
  }

  @Test
  public void updateAllSettingOfHandlingFeeTestWithNullSystemParameter() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.handlingFeeServiceImpl.updateAllSettingOfHandlingFee(null));
  }

}
