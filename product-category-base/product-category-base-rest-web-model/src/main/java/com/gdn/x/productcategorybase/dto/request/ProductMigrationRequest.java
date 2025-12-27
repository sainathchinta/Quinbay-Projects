package com.gdn.x.productcategorybase.dto.request;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMigrationRequest extends BaseDTORequest {
  private String productCode;
  private String updatedStatus;
  private String currentStatus;
  private String migrationType;
  private String migrationPayload;
  private String errorMessage;
}
