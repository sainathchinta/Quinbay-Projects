package com.gdn.aggregate.platform.module.product.listener.model.custom;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = Collections.PCB_CATEGORIES)
public class CustomPcbCategory extends BaseData {

  private String categoryCode;

  private String name;

  private Integer sequence;

  private byte[] description;

  private boolean display;

  private Integer logisticAdjustment;

  private boolean warranty;

  private boolean needIdentity;

  private boolean activated;

  private boolean viewable;

  private String catalogType;

  private String parentCategoryId;

  private String categoryId;

  private String topCategoryId;

  private Integer level;

  private List<MiniPcbCategory> breadCrumbCategories;

  private List<String> parents;

  private String breadcrumb;

  private String url;

  private String imageUrl;

  private String backgroundImage;

  private String subTitle;

  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class MiniPcbCategory {

    private String id;

    private String categoryCode;

    private String name;

    private Integer level;

  }

}
