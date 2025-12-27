package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.product.dao.api.PickupPointRepository;
import com.gdn.x.product.model.entity.PickupPoint;

public class PickupPointServiceImplTest {

  private static final String STORE_ID = "store-id";
  private static final String PICKUP_POINT = "pickup-point";
  private static final String USERNAME = "username";

  @InjectMocks
  private PickupPointServiceImpl pickupPointServiceImpl;

  @Mock
  private PickupPointRepository pickupPointRepository;
  private List<PickupPoint> pickupPoints;
  private PickupPoint pickupPoint;


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    this.pickupPoints = new ArrayList<>();
    this.pickupPoint = new PickupPoint();
    this.pickupPoint.setPickupPointCode(PICKUP_POINT);
    this.pickupPoint.setCncActivated(true);
    this.pickupPoints.add(pickupPoint);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.pickupPointRepository);
  }

  @Test
  public void upsertPickupPointTest() throws Exception {
    this.pickupPointServiceImpl.upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
    verify(this.pickupPointRepository).upsertPickupPoint(STORE_ID, pickupPoint, USERNAME);
  }

  @Test
  public void upsertPickupPointTest_blankStoreId() throws Exception {
    try {
      pickupPointServiceImpl.upsertPickupPoint(null, pickupPoint, USERNAME);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          PickupPointServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void upsertPickupPointTest_nullPickupPoint() throws Exception {
    try {
      pickupPointServiceImpl.upsertPickupPoint(STORE_ID, null, USERNAME);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          PickupPointServiceImpl.PICKUP_POINT_MUST_NOT_BE_NULL, e.getMessage());
    }
  }

  @Test
  public void upsertPickupPointTest_blankUsername() throws Exception {
    try {
      pickupPointServiceImpl.upsertPickupPoint(STORE_ID, pickupPoint, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          PickupPointServiceImpl.USERNAME_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findPickupPointListByPickupPointCodeInAndCncActivatedIsTrueTest() throws Exception {
    this.pickupPointServiceImpl
        .findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID,
            Arrays.asList(PICKUP_POINT));
    verify(this.pickupPointRepository)
        .findByStoreIdAndPickupPointCodeInAndCncActivatedFalse(STORE_ID,
            Arrays.asList(PICKUP_POINT));
  }

  @Test
  public void findPickupPointListByPickupPointCodeInAndCncActivatedIsTrueTest_blankStoreId()
      throws Exception {
    try {
      pickupPointServiceImpl.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(null,
          Arrays.asList(PICKUP_POINT));
    } catch (Exception e) {
      assertEquals(
          ErrorCategory.VALIDATION.getMessage() + PickupPointServiceImpl.STORE_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
    }
  }

  @Test
  public void findPickupPointListByPickupPointCodeInAndCncActivatedIsTrueTest_blankPickupPointCodeList()
      throws Exception {
    try {
      pickupPointServiceImpl
          .findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage()
          + PickupPointServiceImpl.PICKUP_POINT_CODE_LIST_MUST_NOT_BE_NULL, e.getMessage());
    }
  }
}
