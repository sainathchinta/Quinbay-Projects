package com.gdn.aggregate.platform.module.product.listener.model.sub;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Category {

    private String id;

    private String code;

    private String name;

    private Integer level;

}