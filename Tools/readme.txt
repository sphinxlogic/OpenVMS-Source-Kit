- BACKUP savesets from the OpenVMS 7.x supplement must have their attributes
reset after download. A DCL script to do this can be downloaded from

	http://h71000.www7.hp.com/freeware/reset_backup_saveset_file_attributes.zip

or

	http://decuslib.com/decus/freewarev70/000TOOLS/reset_backup_saveset_file_attributes.com

Alternatively, you may use
$ SET FILE/ATTRIBUTES=(RFM:FIX,MRS:32256,LRL:32256,RAT:NONE) file.bck
