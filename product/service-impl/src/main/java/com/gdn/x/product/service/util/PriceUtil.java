package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;

import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;

/**
 * Utility class for price-related operations.
 * Contains static methods that were previously in service classes to avoid circular dependencies.
 */
public class PriceUtil {

    private static final String PRICE_MUST_NOT_BE_NULL = "price must not be null";
    private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

    /**
     * Converts a Price entity to PriceHistory entity.
     * 
     * @param price the price to convert
     * @param itemSku the item SKU
     * @return the converted PriceHistory
     */
    public static PriceHistory convertToPriceHistory(Price price, String itemSku) {
        checkArgument(price != null, PRICE_MUST_NOT_BE_NULL);
        checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);

        PriceHistory priceHistory = new PriceHistory();
        BeanUtils.copyProperties(price, priceHistory);
        priceHistory.setItemSku(itemSku);
        return priceHistory;
    }

    /**
     * Processes discount prices by priority and returns the highest priority discount.
     * 
     * @param prices list of discount prices
     * @return the highest priority discount price or null if none found
     */
    public static DiscountPrice processDiscountPricesByPriority(List<DiscountPrice> prices) {
        if (CollectionUtils.isEmpty(prices)) {
            return null;
        }
        Date now = new Date();
        Optional<List<DiscountPrice>> highestPriorityList = prices.stream()
            .filter(discount -> (discount.getStartDateTime().before(now) && discount.getEndDateTime()
                .after(now))).collect(Collectors.groupingBy(DiscountPrice::getPriority)).entrySet()
            .stream().sorted(Comparator.comparingInt(Map.Entry::getKey)).map(Map.Entry::getValue)
            .findFirst();
        if (highestPriorityList.isPresent()) {
            Double finalDiscountPrice = 0.0;
            DiscountPrice discountPriceFinal = new DiscountPrice();
            for (DiscountPrice discountPrice : highestPriorityList.get()) {
                finalDiscountPrice += discountPrice.getDiscountPrice();
                BeanUtils.copyProperties(discountPrice, discountPriceFinal);
                discountPriceFinal.setDiscountPrice(finalDiscountPrice);
            }
            return discountPriceFinal;
        }
        return null;
    }

    /**
     * Gets the discount price from a Price entity.
     * 
     * @param price the price entity
     * @return the discount price amount
     */
    public static double getDiscountPrice(Price price) {
        DiscountPrice discountPrice = processDiscountPricesByPriority(price.getListOfDiscountPrices());
        double finalPrice = 0.0;
        if (Objects.nonNull(discountPrice)) {
            finalPrice = discountPrice.getDiscountPrice();
        }
        return finalPrice;
    }
}
