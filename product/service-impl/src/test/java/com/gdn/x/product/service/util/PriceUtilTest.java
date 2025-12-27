package com.gdn.x.product.service.util;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;

class PriceUtilTest {

    private static final String TEST_ITEM_SKU = "TEST-ITEM-SKU";
    private static final String TEST_CURRENCY = "IDR";
    private static final String TEST_CHANNEL = "DEFAULT";
    private static final String TEST_UPDATED_BY = "TEST-USER";
    private static final String TEST_ADJUSTMENT_NAME = "PROMO";
    private static final Double TEST_BASE_PRICE = 100000.0;
    private static final Double TEST_LIST_PRICE = 110000.0;
    private static final Double TEST_DISCOUNT_PRICE = 10000.0;
    private static final Double TEST_DISCOUNT_PRICE_2 = 5000.0;
    private static final Integer TEST_PRIORITY = 1;
    private static final Integer TEST_PRIORITY_2 = 2;

    private Price price;
    private DiscountPrice discountPrice;
    private Date now;
    private Date yesterday;
    private Date tomorrow;

    @BeforeEach
    void setUp() {
        Calendar cal = Calendar.getInstance();
        now = cal.getTime();
        
        cal.add(Calendar.DATE, -1);
        yesterday = cal.getTime();
        
        cal.add(Calendar.DATE, 2);
        tomorrow = cal.getTime();

        price = new Price();
        price.setCurrency(TEST_CURRENCY);
        price.setListPrice(TEST_LIST_PRICE);
        price.setOfferPrice(TEST_BASE_PRICE);
        price.setChannel(TEST_CHANNEL);
        price.setLastUpdatedBy(TEST_UPDATED_BY);

        discountPrice = new DiscountPrice();
        discountPrice.setDiscountPrice(TEST_DISCOUNT_PRICE);
        discountPrice.setPriority(TEST_PRIORITY);
        discountPrice.setStartDateTime(yesterday);
        discountPrice.setEndDateTime(tomorrow);
        discountPrice.setAdjustmentName(TEST_ADJUSTMENT_NAME);
    }

    @Test
    void testConvertToPriceHistory_WithValidInputs() {
        PriceHistory result = PriceUtil.convertToPriceHistory(price, TEST_ITEM_SKU);
        
        assertNotNull(result);
        assertEquals(TEST_ITEM_SKU, result.getItemSku());
        assertEquals(TEST_CURRENCY, result.getCurrency());
        assertEquals(TEST_BASE_PRICE, result.getOfferPrice());
        assertEquals(TEST_LIST_PRICE, result.getListPrice());
        assertEquals(TEST_CHANNEL, result.getChannel());
        assertEquals(TEST_UPDATED_BY, result.getLastUpdatedBy());
    }

    @Test
    void testConvertToPriceHistory_WithNullPrice() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            PriceUtil.convertToPriceHistory(null, TEST_ITEM_SKU);
        });
    }

    @Test
    void testConvertToPriceHistory_WithBlankItemSku() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            PriceUtil.convertToPriceHistory(price, "");
        });
    }

    @Test
    void testConvertToPriceHistory_WithNullItemSku() {
        assertThrows(ApplicationRuntimeException.class, () -> {
            PriceUtil.convertToPriceHistory(price, null);
        });
    }

    @Test
    void testProcessDiscountPricesByPriority_WithNullList() {
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(null);
        
        assertNull(result);
    }

    @Test
    void testProcessDiscountPricesByPriority_WithEmptyList() {
        List<DiscountPrice> emptyList = new ArrayList<>();
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(emptyList);
        
        assertNull(result);
    }

    @Test
    void testProcessDiscountPricesByPriority_WithSingleActiveDiscount() {
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNotNull(result);
        assertEquals(TEST_DISCOUNT_PRICE, result.getDiscountPrice());
        assertEquals(TEST_PRIORITY, result.getPriority());
        assertEquals(TEST_ADJUSTMENT_NAME, result.getAdjustmentName());
    }

    @Test
    void testProcessDiscountPricesByPriority_WithExpiredDiscount() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DATE, -10);
        Date pastStart = cal.getTime();
        
        cal.add(Calendar.DATE, 5);
        Date pastEnd = cal.getTime();
        
        discountPrice.setStartDateTime(pastStart);
        discountPrice.setEndDateTime(pastEnd);
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNull(result);
    }

    @Test
    void testProcessDiscountPricesByPriority_WithFutureDiscount() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DATE, 5);
        Date futureStart = cal.getTime();
        
        cal.add(Calendar.DATE, 5);
        Date futureEnd = cal.getTime();
        
        discountPrice.setStartDateTime(futureStart);
        discountPrice.setEndDateTime(futureEnd);
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNull(result);
    }

    @Test
    void testProcessDiscountPricesByPriority_WithMultipleDiscountsSamePriority() {
        DiscountPrice discount2 = new DiscountPrice();
        discount2.setDiscountPrice(TEST_DISCOUNT_PRICE_2);
        discount2.setPriority(TEST_PRIORITY);
        discount2.setStartDateTime(yesterday);
        discount2.setEndDateTime(tomorrow);
        discount2.setAdjustmentName(TEST_ADJUSTMENT_NAME);
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        discounts.add(discount2);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNotNull(result);
        assertEquals(TEST_DISCOUNT_PRICE + TEST_DISCOUNT_PRICE_2, result.getDiscountPrice());
        assertEquals(TEST_PRIORITY, result.getPriority());
    }

    @Test
    void testProcessDiscountPricesByPriority_WithMultipleDiscountsDifferentPriority() {
        DiscountPrice discount2 = new DiscountPrice();
        discount2.setDiscountPrice(TEST_DISCOUNT_PRICE_2);
        discount2.setPriority(TEST_PRIORITY_2);
        discount2.setStartDateTime(yesterday);
        discount2.setEndDateTime(tomorrow);
        discount2.setAdjustmentName(TEST_ADJUSTMENT_NAME);
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        discounts.add(discount2);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNotNull(result);
        assertEquals(TEST_DISCOUNT_PRICE, result.getDiscountPrice());
        assertEquals(TEST_PRIORITY, result.getPriority());
    }

    @Test
    void testProcessDiscountPricesByPriority_WithMixedActiveAndInactive() {
        DiscountPrice inactiveDiscount = new DiscountPrice();
        inactiveDiscount.setDiscountPrice(TEST_DISCOUNT_PRICE_2);
        inactiveDiscount.setPriority(TEST_PRIORITY);
        
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DATE, -10);
        inactiveDiscount.setStartDateTime(cal.getTime());
        cal.add(Calendar.DATE, 5);
        inactiveDiscount.setEndDateTime(cal.getTime());
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        discounts.add(inactiveDiscount);
        
        DiscountPrice result = PriceUtil.processDiscountPricesByPriority(discounts);
        
        assertNotNull(result);
        assertEquals(TEST_DISCOUNT_PRICE, result.getDiscountPrice());
    }

    @Test
    void testGetDiscountPrice_WithValidDiscounts() {
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        price.setListOfDiscountPrices(discounts);
        
        double result = PriceUtil.getDiscountPrice(price);
        
        assertEquals(TEST_DISCOUNT_PRICE, result);
    }

    @Test
    void testGetDiscountPrice_WithNullDiscounts() {
        price.setListOfDiscountPrices(null);
        
        double result = PriceUtil.getDiscountPrice(price);
        
        assertEquals(0.0, result);
    }

    @Test
    void testGetDiscountPrice_WithEmptyDiscounts() {
        price.setListOfDiscountPrices(new ArrayList<>());
        
        double result = PriceUtil.getDiscountPrice(price);
        
        assertEquals(0.0, result);
    }

    @Test
    void testGetDiscountPrice_WithNoActiveDiscounts() {
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DATE, -10);
        discountPrice.setStartDateTime(cal.getTime());
        cal.add(Calendar.DATE, 5);
        discountPrice.setEndDateTime(cal.getTime());
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        price.setListOfDiscountPrices(discounts);
        
        double result = PriceUtil.getDiscountPrice(price);
        
        assertEquals(0.0, result);
    }

    @Test
    void testGetDiscountPrice_WithMultipleActiveDiscountsSamePriority() {
        DiscountPrice discount2 = new DiscountPrice();
        discount2.setDiscountPrice(TEST_DISCOUNT_PRICE_2);
        discount2.setPriority(TEST_PRIORITY);
        discount2.setStartDateTime(yesterday);
        discount2.setEndDateTime(tomorrow);
        
        List<DiscountPrice> discounts = new ArrayList<>();
        discounts.add(discountPrice);
        discounts.add(discount2);
        price.setListOfDiscountPrices(discounts);
        
        double result = PriceUtil.getDiscountPrice(price);
        
        assertEquals(TEST_DISCOUNT_PRICE + TEST_DISCOUNT_PRICE_2, result);
    }
}

