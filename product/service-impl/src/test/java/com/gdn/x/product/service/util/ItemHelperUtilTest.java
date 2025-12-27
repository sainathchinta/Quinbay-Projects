package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;

class ItemHelperUtilTest {

  private static final String TEST_PICKUP_POINT_CODE_1 = "PP001";
  private static final String TEST_PICKUP_POINT_CODE_2 = "PP002";
  private static final String TEST_PICKUP_POINT_CODE_3 = "PP003";
  private static final String TEST_COMMA_SEPARATED_CODES = "PP001,PP002,PP003";
  private static final String TEST_EMPTY_STRING = "";

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithValidInputs() {
    String pickupPointCodeFilter = TEST_PICKUP_POINT_CODE_1;
    List<String> pickupPointCodesFilter = Arrays.asList(TEST_PICKUP_POINT_CODE_2, TEST_PICKUP_POINT_CODE_3);

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        pickupPointCodeFilter, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(3, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_2));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_3));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithNullPickupPointCodesFilter() {
    String pickupPointCodeFilter = TEST_PICKUP_POINT_CODE_1;

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        pickupPointCodeFilter, null);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithNullPickupPointCodeFilter() {
    List<String> pickupPointCodesFilter = Arrays.asList(TEST_PICKUP_POINT_CODE_1, TEST_PICKUP_POINT_CODE_2);

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        null, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(2, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_2));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithBothNull() {
    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        null, null);

    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithEmptyPickupPointCodesFilter() {
    String pickupPointCodeFilter = TEST_PICKUP_POINT_CODE_1;
    List<String> pickupPointCodesFilter = new ArrayList<>();

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        pickupPointCodeFilter, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(1, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithCommaSeparatedFilter() {
    List<String> pickupPointCodesFilter = Arrays.asList(TEST_PICKUP_POINT_CODE_1);

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        TEST_COMMA_SEPARATED_CODES, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(4, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_2));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_3));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithEmptyStringFilter() {
    List<String> pickupPointCodesFilter = Arrays.asList(TEST_PICKUP_POINT_CODE_1, TEST_PICKUP_POINT_CODE_2);

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        TEST_EMPTY_STRING, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(3, result.size());
    assertTrue(result.contains(TEST_EMPTY_STRING));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_2));
  }

  @Test
  void testCombinePickupPointCodeAndPickupPointCodesFilter_WithDuplicateValues() {
    String pickupPointCodeFilter = TEST_PICKUP_POINT_CODE_1;
    List<String> pickupPointCodesFilter = Arrays.asList(TEST_PICKUP_POINT_CODE_1, TEST_PICKUP_POINT_CODE_2);

    List<String> result = ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(
        pickupPointCodeFilter, pickupPointCodesFilter);

    assertNotNull(result);
    assertEquals(3, result.size());
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_1));
    assertTrue(result.contains(TEST_PICKUP_POINT_CODE_2));
  }
}
