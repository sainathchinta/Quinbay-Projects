package com.gdn.x.product.model.entity;

import java.io.Serial;

import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.x.product.enums.ProductFieldNames;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Video implements GdnBaseEmbedded {
  @Serial
  private static final long serialVersionUID = -6193210594433869250L;

  @Field(value = ProductFieldNames.VIDEO_ID)
  private String videoId;

  @Field(value = ProductFieldNames.VIDEO_URL)
  private String finalUrl;

  @Field(value = ProductFieldNames.VIDEO_COVER_IMAGE_PATH)
  private String coverImagePath;

  @Field(value = ProductFieldNames.VIDEO_SOURCE_URL)
  private String sourceUrl;

  @Field(value = ProductFieldNames.VIDEO_NAME)
  private String videoName;
}
