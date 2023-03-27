package uk.co.nstauthority.scap.scap.summary;

import java.math.BigDecimal;
import java.util.List;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;
import uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryView;

public record ProjectDetailsSummaryView(String projectName,
                                        String projectSummary,
                                        List<ProjectType> projectTypes,
                                        BigDecimal projectCostEstimate,
                                        BigDecimal estimatedValueLocalContent,
                                        List<String> fieldNames,
                                        YesNo hasFacilities,
                                        List<String> projectFacilities,
                                        String plannedExecutionStartDate,
                                        String plannedCompletionDate,
                                        List<FileUploadSummaryView> supportingDocuments) {
}
