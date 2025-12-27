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
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.PRODUCT_REVIEW)
public class CustomProductReview extends BaseData {

    private String metaDataType;

    private int averageRating;

    private int[] ratings;

    private double[] ratingPercentages;

    private int reviewCount;

    private double decimalRating;

}
