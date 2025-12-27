package com.gdn.partners.pcu.external.web.configuration;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum TracerFieldKey {

    CHANNEL_ID("channelId"),
    CLIENT_ID("clientId"),
    STORE_ID("storeId"),
    USER_NAME("username"),
    REQUEST_ID("requestId"),
    IS_EXTERNAL("isExternal"),
    BUSINESS_PARTNER_CODE("businessPartnerCode"),
    BUSINESS_PARTNER_NAME("businessPartnerName"),
    EXTERNAL_USER_MODE("STORE"),
    USER_EMAIL("userEmail"),
    CLIENT_HOST("clientHost"),
    EMAIL("email"),
    IS_EXTERNAL_ONLY("isExternalOnly"),
    LINKED_BUSINESS_PARTNER_CODE("linkedStoreCode"),
    APP_TYPE("appType"),
    APP_VERSION("appVersion"),
    PICKUP_POINTS("pickupPoints"),
    CLIENT_TYPE("clientType"),
    IS_PRODUCT_VIDEO_ACTIVATED("isProductVideoActivated");

    private static final List<String> KEYS = (List) Arrays.stream(values()).map(TracerFieldKey::getKey).collect(Collectors.toList());

    private String key;
    private TracerFieldKey(String key) {
        this.key = key;
    }

    public static List<String> getKeys() {
        return KEYS;
    }

    public String getKey() {
        return this.key;
    }
}
