package com.gda.mta.product.dto;

import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import java.util.Set;

/**
 * Defines the contract for Master Data Update Requests.
 * This interface represents the structure for updating product master data,
 */
public interface MasterDataUpdateRequest {

    // Basic Product Information
    String getDescription();
    /**
     * Sets the description for the product.
     *
     * @param description the description to set
     */
    void setDescription(String description);

    String getUrl();
    /**
     * Sets the YouTube URL for the product.
     *
     * @param url the URL to set
     */
    void setUrl(String url);

    String getProductCode();

    String getProductName();

    Integer getProductType();

    // Change Tracking
    Set<L3InfoUpdateChangeType> getMasterDataEditChangeTypes();

    // Product Dimensions
    Double getLength();
    /**
     * Sets the length for the product.
     *
     * @param length the length to set
     */
    void setLength(Double length);

    Double getWidth();
    /**
     * Sets the width for the product.
     *
     * @param width the width to set
     */
    void setWidth(Double width);

    Double getHeight();
    /**
     * Sets the height for the product.
     *
     * @param height the height to set
     */
    void setHeight(Double height);

    // Product Weights
    Double getWeight();
    /**
     * Sets the weight for the product.
     *
     * @param weight the weight to set
     */
    void setWeight(Double weight);

    Double getShippingWeight();
    /**
     * Sets the shipping weight for the product.
     *
     * @param shippingWeight the shipping weight to set
     */
    void setShippingWeight(Double shippingWeight);

    // Instore Settings
    boolean isOff2OnChannelActive();
    boolean isPureInstoreProduct();
}
