package uk.co.nstauthority.scap.energyportal;

import java.util.Arrays;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.generated.client.FieldsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.fivium.energyportalapi.generated.types.FieldStatus;

@Service
public class FieldQueryService {

  private static final FieldsProjectionRoot FIELDS_PROJECTION_ROOT = new FieldsProjectionRoot().fieldId().fieldName();

  private final EnergyPortalApiWrapper energyPortalApiWrapper;
  private final FieldApi fieldApi;

  @Autowired
  public FieldQueryService(EnergyPortalApiWrapper energyPortalApiWrapper, FieldApi fieldApi) {
    this.energyPortalApiWrapper = energyPortalApiWrapper;
    this.fieldApi = fieldApi;
  }

  public List<FieldSelectable> getFieldSearchableResults(String fieldName) {
    return energyPortalApiWrapper.makeRequest((logCorrelationId, requestPurpose) ->
        searchFields(
            fieldName,
            requestPurpose.purpose(),
            logCorrelationId.id())
            .stream()
            .map(this::fromField)
            .toList());
  }

  private FieldSelectable fromField(Field field) {
    var fieldSelectable = new FieldSelectable();
    fieldSelectable.setFieldId(String.valueOf(field.getFieldId()));
    fieldSelectable.setFieldName(field.getFieldName());
    return fieldSelectable;
  }

  private List<Field> searchFields(String fieldName, String requestPurpose, String logCorrelationId) {
    var statuses = Arrays.stream(FieldStatus.values())
        .filter(fieldStatus -> fieldStatus != FieldStatus.STATUS9999)
        .toList();
    return fieldApi.searchFields(
        fieldName,
        statuses,
        FIELDS_PROJECTION_ROOT,
        requestPurpose,
        logCorrelationId);
  }
}
