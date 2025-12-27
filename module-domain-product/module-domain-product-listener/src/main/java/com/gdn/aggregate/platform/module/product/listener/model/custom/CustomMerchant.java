package com.gdn.aggregate.platform.module.product.listener.model.custom;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = Collections.MERCHANT)
public class CustomMerchant extends BaseData {

    private String name;

    private String alias;

    private String type;

    private String deliveryType;

    private boolean internationalFlag;

    private String logo;

    private boolean official;

    private String contractLevel;

    private String badge;

    private Integer rating;

    private Integer positiveFeedbackRating;

    private Integer fastFulfillmentRating;

    private Integer fastCustomerResponseRating;

    private String badgeImageUrl;

}
