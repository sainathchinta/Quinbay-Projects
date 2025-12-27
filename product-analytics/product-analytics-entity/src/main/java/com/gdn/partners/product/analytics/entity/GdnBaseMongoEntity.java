package com.gdn.partners.product.analytics.entity;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.gdn.partners.product.analytics.model.Constants;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.GenericGenerator;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Field;

import javax.persistence.GeneratedValue;
import java.io.Serializable;
import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
public abstract class GdnBaseMongoEntity implements Serializable {

  private static final long serialVersionUID = -3545741791679645477L;

  @Id
  @GeneratedValue(generator = "system-uuid")
  @GenericGenerator(name = "system-uuid", strategy = "uuid2")
  private String id;

  @Version
  @Field(value = Constants.VERSION)
  private Long version;

  @CreatedDate
  @Field(value = Constants.CREATED_DATE)
  private Date createdDate;

  @CreatedBy
  @JsonAlias(Constants.CREATED_BY)
  @Field(value = Constants.CREATED_BY)
  private String createdBy;

  @LastModifiedDate
  @Field(value = Constants.UPDATED_DATE)
  private Date updatedDate;

  @LastModifiedBy
  @JsonAlias(Constants.UPDATED_BY)
  @Field(value = Constants.UPDATED_BY)
  private String updatedBy;

  @Field(value = Constants.STORE_ID)
  private String storeId;

  @Field(value = Constants.MARK_FOR_DELETE)
  private boolean markForDelete;
}
